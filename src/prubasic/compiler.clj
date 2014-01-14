(ns prubasic.compiler
  (:require [prubasic.parser :refer [parse2]]
            [prubasic.core :refer [ldi add mov qbgt nop0 sbco]]))

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

(defn alloc [r k allocations instr]
  {:pre [r k]
   :post [(or (contains? (:register-allocation (:env %)) (get instr k))
              (keyword? (get instr k)))
          (every? (complement keyword?) (keys (:register-allocation (:env %))))
          (= allocations (select-keys (:register-allocation (:env %) {}) (keys allocations)))]}
  (if (keyword? (get instr k))
    (assoc-in instr [:env :register-allocation] {})
    (-> (if (contains? allocations (get instr k))
          (assoc instr k (get allocations (get instr k)))
          (assoc instr k r))
        (update-in [:env :register-allocation] merge {(get instr k) r} allocations))))

(def allocate-registers-for-instruction nil)

(defmulti allocate-registers-for-instruction (fn [registers allocations instr] (:op instr)))

(defmethod allocate-registers-for-instruction :ldi [registers allocations instr]
  (alloc (first registers) :operand1 allocations instr))

(defmethod allocate-registers-for-instruction :sbbo [registers allocations instr]
  {:post [(every? (complement keyword?) (keys (:register-allocation (:env %))))]}
  (assert (keyword? (:operand1 instr)))
  (assert (keyword? (:operand2 instr)))
  (update-in instr [:env :register-allocation] merge allocations))

(defmethod allocate-registers-for-instruction :add [registers allocations instr]
  {:post [(every? (complement keyword?) (keys (:register-allocation (:env %))))]}
  (let [[r1 r2 r3] (seq registers)
        i (alloc r1 :operand1 allocations instr)
        i (alloc r2 :operand2 (:register-allocation (:env i)) i)
        i (alloc r3 :operand3 (:register-allocation (:env i)) i)]
    i))

(defmethod allocate-registers-for-instruction :mov [registers allocations instr]
  {:post [(every? (complement keyword?) (keys (:register-allocation (:env %))))]}
  (assert (contains? allocations (:operand2 instr)) (:operand2 instr))
  (let [[r1 r2] (seq registers)]
    (->> (alloc r1 :operand1 allocations instr)
         (alloc r2 :operand2 allocations))))

(defmethod allocate-registers-for-instruction :qbgt [registers allocations instr]
  {:post [(every? (complement keyword?) (keys (:register-allocation (:env %))))]}
  (assert (contains? allocations (:operand2 instr)) (:operand2 instr))
  (let [[r1] (seq registers)]
    (alloc r1 :operand2 allocations instr)))

(defmethod allocate-registers-for-instruction :nop0 [registers allocations instr]
  {:post [(every? (complement keyword?) (keys (:register-allocation (:env %))))]}
  (update-in instr [:env :register-allocation] merge allocations))

(defmethod allocate-registers-for-instruction :halt [registers allocations instr]
  {:post [(every? (complement keyword?) (keys (:register-allocation (:env %))))]}
  (update-in instr [:env :register-allocation] merge allocations))

(defmethod allocate-registers-for-instruction :sbco [registers allocations instr]
  (assert (contains? allocations (:operand1 instr)) (:operand1 instr))
  (let [[r1 r2] (seq registers)]
    (->> (alloc r1 :operand1 allocations instr)
         (alloc r2 :operand3 allocations))))

(defn allocate-registers [ast]
  (:result
   (reduce
    (fn [{:keys [available-registers result]} instr]
      (let [{:keys [last-reads]} instr
            new-free-registers (select-keys (:register-allocation (:env instr)) last-reads)
            allocations (if (empty? result)
                          {}
                          (:register-allocation (:env (peek result))))
            r (allocate-registers-for-instruction available-registers allocations instr)
            r (reduce (fn [r n] (update-in r [:env :register-allocation] dissoc n)) r last-reads)
            available-registers (reduce
                                 disj
                                 available-registers
                                 (vals (:register-allocation (:env r))))]
        {:available-registers (into available-registers (remove keyword? (vals new-free-registers)))
         :result (conj result r)}))
    {:available-registers registers
     :result []}
    ast)))

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
