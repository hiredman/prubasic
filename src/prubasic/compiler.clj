(ns prubasic.compiler
  (:require [prubasic.parser :refer [parse2]]
            [prubasic.core :refer [ldi add mov qbgt nop0 sbco halt]]
            [prubasic.passes.registers :refer [allocate-registers]]))

;; http://glind.customer.netspace.net.au/gwbas-17.html

(def registers (set (for [i (range 1 31)] (keyword (str "r" i)))))

(def analyze nil)
(defmulti analyze (fn [env [_ _ [command]]] command))

(defn linearize-expression [env [_ [thing-type :as thing] :as exp]]
  (case thing-type
    :variable-name (let [[_ n] thing]
                     [n])
    :value (let [[_ [_ _ [_ digits]]] thing]
             [(Long/parseLong digits 16)])
    :expression (let [[_ a [_ op] b] exp]
                  (into (into (linearize-expression env a)
                              (linearize-expression env b))
                        [(keyword op)]))))

(defn expression-rewrite [env target-r x]
  (loop [to-push (seq (linearize-expression env x))
         stack []
         result []]
    (if (empty? to-push)
      (if (number? (peek stack))
        (conj result (ldi target-r (peek stack) env))
        (conj result (mov target-r (peek stack) env)))
      (let [[x & xs] to-push]
        (case x
          :+ (let [a (peek stack)
                   stack (pop stack)
                   b (peek stack)
                   stack (pop stack)
                   stack (conj stack :r2)]
               (case [(number? a) (number? b)]
                 [true true] (let [ar (gensym 'a)
                                   br (gensym 'b)
                                   cr (gensym 'c)]
                               (recur xs
                                      (conj stack cr)
                                      (into result
                                            [(ldi ar a env)
                                             (ldi br b env)
                                             (add cr ar br env)])))
                 [true false] (let [ar (gensym 'a)
                                    cr (gensym 'c)]
                                (recur xs
                                       (conj stack cr)
                                       (into result
                                             [(ldi ar a env)
                                              (add cr ar b env)])))
                 [false true] (let [br (gensym 'b)
                                    cr (gensym 'c)]
                                (recur xs
                                       (conj stack cr)
                                       (into result
                                             [(ldi br b env)
                                              (add cr a br env)])))
                 [false false] (let [cr (gensym 'c)]
                                 (recur xs
                                        (conj stack cr)
                                        (into result
                                              [(add cr a b env)])))))
          (recur xs (conj stack x) result))))))


(defmethod analyze :let [env [_ [_ label] [_ [_ vn] expression]]]
  (assoc-in (expression-rewrite env vn expression) [0 :label] label))

(defmethod analyze :for [env [_ [_ label] [_ [_ variable-name] [thing-type :as thing] [_ [_ _ [_ digits]]]]]]
  (case thing-type
    :value (let [[_ [_ _ [_ thing-digits]]] thing
                 thing (Long/parseLong thing-digits 16)
                 limit (Long/parseLong digits 16)
                 next-target (gensym 'for)
                 end (gensym 'forend)
                 new-env (assoc-in env [:for variable-name :target] next-target)
                 new-env (assoc-in new-env [:for variable-name :limit] limit)]
             [(ldi variable-name thing new-env label)
              ;; nop
              (mov variable-name variable-name new-env next-target)])))

(defmethod analyze :next [env [_ [_ label] [_ [_ vn]]]]
  (let [tmp (gensym 'temp)]
    [(ldi tmp 1 env label)
     (add vn vn tmp env)
     (qbgt (get-in env [:for vn :target])
           vn
           (get-in env [:for vn :limit])
           (update-in env [:for] dissoc vn))]))

(defmethod analyze :write [env [_ [_ label] [_ [_ vn] exp]]]
  (let [temp (gensym 'tmp)]
    (vec
     (concat
      [(nop0 env label)]
      (expression-rewrite env temp exp)
      [(sbco vn :c24 temp 0x4 env)]))))

(def PRU0-ARM-INTERRUPT 19)

(defmethod analyze :end [env [_ [_ label]]]
  [(nop0 env label)
   (ldi :r31.b0 (+ PRU0-ARM-INTERRUPT 16) env)
   (halt env)])

(def prelude
  [{:op :ldi
    :operand1 :r1
    :operand2 0x0
    :env {}
    :label nil}
   {:op :ldi
    :operand1 :r1.w1
    :operand2  0x2
    :env {}
    :label nil}
   {:op :ldi
    :operand1 :r1.w0
    :operand2  0x2020
    :env {}
    :label nil}
   {:op :sbbo
    :operand1 :r1
    :operand2 :r1
    :operand3 0x0
    :operand4 0x4
    :label nil
    :env {}}])

(defn analyze-program [env program]
  (:result
   (reduce
    (fn [{:keys [result env]} command-line]
      (let [x (analyze env command-line)]
        {:result (into result x)
         :env (:env (last x))}))
    {:result prelude :env env}
    program)))

(defn tag-last-usage [ast]
  (loop [i (dec (count ast))
         seen #{}
         ast ast]
    (if (neg? i)
      ast
      (let [reads (:reads (nth ast i))
            last-read (set (remove seen reads))]
        (if-not (seq last-read)
          (recur (dec i) seen ast)
          (recur (dec i) (into seen last-read)
                 (assoc-in ast [i :last-reads] last-read)))))))

(defn number-instructions [ast]
  (vec (keep-indexed #(assoc %2 :n %1) ast)))

(defn shift-last-reads-out-of-loops [ast]
  (let [n-by-label (group-by first (map (juxt :label :n) ast))
        i-by-n (group-by :n ast)]
    (reduce
     (fn [ast command-line]
       (assert ast)
       (case (:op command-line)
         (:qbgt) (let [n (get-in n-by-label [(:operand1 command-line) 0 1])]
                   (assert (> (:n command-line) n) "loops jump backwards")
                   (assert (pos? n))
                   (loop [i n
                          last-reads #{}
                          ast ast]
                     (if (= i (:n command-line))
                       (update-in ast [i :last-reads] (fnil into #{}) last-reads)
                       (recur (inc i)
                              (let [inst (nth ast i)]
                                (assert inst)
                                (into last-reads (set (:last-reads inst))))
                              (assoc-in ast [i :last-reads] #{})))))
         ast))
     ast
     ast)))

;; TODO: make all names unique

(defn resolve-labels [s]
  (let [idx (group-by :label s)]
    (vec
     (for [instr s]
       (case (:op instr)
         (:qbgt :qblt :qbne :qba :qbge) (let [x (:operand1 instr)
                                              target-instruction-number (-> idx (get x) first :n)]
                                          (assert target-instruction-number
                                                  {:x x
                                                   '(get idx x) (get idx x)})
                                          (if (number? x)
                                            instr
                                            (assoc instr
                                              :operand1 (- (-> idx (get x) first :n) (:n instr)))))
         instr)))))

(comment
  (prubasic.codegen/code-gen
   (resolve-labels
    (allocate-registers
     (shift-last-reads-out-of-loops
      (number-instructions
       (tag-last-usage
        (analyze-program
         {}
         (parse2
          "
10 LET temp = 0x0
20 LET fib = 0x1
30 FOR number = 0x1 TO 0x10
40   LET pair = temp + fib
50   LET temp = fib
60   LET fib = pair
70 NEXT number
71 WRITE fib 0x0
80 END
"))))))))



  (prubasic.codegen/code-gen
   (resolve-labels
    (allocate-registers
     (shift-last-reads-out-of-loops
      (number-instructions
       (tag-last-usage
        (analyze-program
         {}
         (parse2
          "
30 FOR number = 0x1 TO 0x10
70 NEXT number
71 WRITE number 0x0
80 END
"))))))))




  )
