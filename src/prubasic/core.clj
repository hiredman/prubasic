(ns prubasic.core
  (:require [instaparse.core :as insta]))

(def basic-g
  "
<program> = [endln] program | command-line endln program | command-line [endln]
command-line = digits ws command
<command> = let | for | next | if | goto | end | read | write
let = <'LET '> variable-name <' = '> expression
for = <'FOR '> variable-name <' = '> value <' TO '> value
next = <'NEXT '> variable-name
if = <'IF '> comparison-expression  <' THEN '> command
goto = <'GOTO '> digits
end = <'END'>
read = <'READ '> variable-name <' '> expression
write = <'WRITE '> variable-name <' '> expression

expression = value | variable-name | expression ws operator ws expression
value = hex-number
hex-number = '0x' hex-digits
hex-digits = #'[a-fA-F0-9]+'
digits = #'[0-9]+'
variable-name = #'[a-z][a-zA-Z0-9]*'
<endln> = <'\n'>
<ws> = <#'\\s+'>
operator = '+'
comparison = '='
comparison-expression = expression ws comparison ws expression
")


(def parse (insta/parser basic-g))


;; http://glind.customer.netspace.net.au/gwbas-17.html

(defmulti pass1 (fn [env [_ _ [cmd]]] cmd))

(defn expression-to-stack [env [_ [thing-type :as thing] :as exp]]
  (case thing-type
    :variable-name (let [[_ n] thing
                         r (get-in env [:registers n])]
                     (assert r thing)
                     [r])
    :value (let [[_ [_ _ [_ digits]]] thing]
             [(Long/parseLong digits 16)])
    :expression (let [[_ a [_ op] b] exp]
                  (into (into (expression-to-stack env a)
                              (expression-to-stack env b))
                        [(keyword op)]))))

(defn expression-rewrite [env label target-r x]
  (loop [to-push (seq (expression-to-stack env x))
         stack []
         label label
         result []]
    (if (empty? to-push)
      (if (keyword? (peek stack))
        (conj result {:op :mov
                      :operand1 target-r
                      :operand2 (peek stack)
                      :env env
                      :label label})
        (conj result {:op :ldi
                      :operand1 target-r
                      :operand2 (peek stack)
                      :env env
                      :label label}))
      (let [[x & xs] to-push]
        (case x
          :+ (let [a (peek stack)
                   stack (pop stack)
                   b (peek stack)
                   stack (pop stack)
                   stack (conj stack :r30)]
               (recur xs stack nil (conj result
                                         {:op :add
                                          :operand1 :r30
                                          :operand2 a
                                          :operand3 b
                                          :env env
                                          :label nil})))
          (recur xs (conj stack x) nil result))))))

(defmethod pass1 :let [env [_ [_ label] [_ [_ variable-name] [thing-type :as thing]] :as command-line]]
  (let [r (if (contains? (:registers env)  variable-name)
            (get-in env [:registers variable-name])
            (keyword (str "r" (inc (count (:registers env))))))
        new-env (assoc-in env [:registers variable-name] r)]
    (assoc-in (expression-rewrite new-env label r thing) [0 :label] label)))

(defmethod pass1 :for [env [_ [_ label] [_ [_ variable-name] [thing-type :as thing] [_ [_ _ [_ digits]]]]]]
  (case thing-type
    :value (let [[_ [_ _ [_ thing-digits]]] thing
                 thing (Long/parseLong thing-digits 16)
                 limit (Long/parseLong digits 16)
                 next-target (gensym 'for)
                 end (gensym 'forend)
                 r (keyword (str "r" (inc (count (:registers env)))))
                 new-env (assoc-in env [:registers variable-name] r)
                 new-env (assoc-in new-env [:for r :target] next-target)
                 new-env (assoc-in new-env [:for r :limit] limit)]
             [{:op :ldi
               :operand1 r
               :operand2 thing
               :env new-env
               :label label}
              {:op :mov
               :operand1 :r0
               :operand2 :r0
               :env new-env
               :label next-target}])))

(defmethod pass1 :next [env [_ [_ label] [_ [_ vn]]]]
  (assert (contains? (:registers env) vn) vn)
  [{:op :ldi
    :operand1 :r30
    :operand2 1
    :env env
    :label label}
   {:op :add
    :operand1 (get-in env [:registers vn])
    :operand2 (get-in env [:registers vn])
    :operand3 :r30
    :env env
    :label nil}
   {:op :qbgt
    :operand1 (get-in env [:for (get-in env [:registers vn]) :target])
    :operand2 (get-in env [:registers vn])
    :operand3 (get-in env [:for (get-in env [:registers vn]) :limit])
    :env env
    :label nil}])

(def PRU0-ARM-INTERRUPT 19)

(defmethod pass1 :end [env [_ [_ label]]]
  [{:op :mov
    :operand1 :r0
    :operand2 :r0
    :env env
    :label label}
   {:op :ldi
    :operand1 :r31.b0
    :operand2 (+ PRU0-ARM-INTERRUPT 16)
    :env env
    :label nil}
   {:op :halt
    :env env
    :label nil}])

(defmethod pass1 :write [env [_ [_ label] [_ [_ vn] exp]]]
  (let [source (get-in env [:registers vn])]
    (vec
     (concat
      [{:op :nop0
        :operand1 :r0
        :operand2 :r1
        :operand3 :r2
        :env env
        :label label}]
      (expression-rewrite env nil :r30 exp)
      [{:op :sbco
        :operand1 source
        :operand2 :c24
        :operand3 :r30
        :operand4 0x4
        :label nil
        :env env}]))))

(defmethod pass1 :if [env [_ [_ label] [_ [_ a op b] thing]]]
  (case [(second op) (first (second a)) (first (second b))]
    ["=" :variable-name :value] (let [r (get-in env [:registers (second (second a))])
                                      neq (gensym 'neq)]
                                  (concat [{:op :ldi
                                            :operand1 :r30
                                            :operand2 (Long/parseLong
                                                       (-> b second second last second)
                                                       16)
                                            :label nil
                                            :env env}
                                           {:op :qbne
                                            :operand1 neq
                                            :operand2 r
                                            :operand3 :r30
                                            :label label
                                            :env env}]
                                          (pass1 env [nil [_ nil] thing])
                                          [{:op :nop0
                                            :operand1 :r1
                                            :operand2 :r2
                                            :operand3 :r0
                                            :env env
                                            :label neq}]))))

(defmethod pass1 :goto [env [_ [_ label] [_ [_ target-label]]]]
  [{:op :qba
    :operand1 target-label
    :env env
    :label label}])

(defn resolve-labels [s]
  (let [idx (group-by :label s)]
    (for [instr s]
      (case (:op instr)
        (:qbgt :qblt :qbne :qba) (let [x (:operand1 instr)
                                       target-instruction-number (-> idx (get x) first :n)]
                                   (assert target-instruction-number
                                           {:x x
                                            '(get idx x) (get idx x)})
                                   (if (number? x)
                                     instr
                                     (assoc instr
                                       :operand1 (- (-> idx (get x) first :n) (:n instr)))))
        instr))))

(defn f [s]
  (resolve-labels
   (keep-indexed
    #(assoc %2 :n %1)
    (concat
     [{:op :ldi
       :operand1 :r0
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
       :operand1 :r0
       :operand2 :r1
       :operand3 0x0
       :operand4 0x4
       :label nil
       :env {}}]
     (rest
      (reduce
       (fn [result command-line]
         (let [e (:env (peek result))]
           (into result (pass1 e command-line))))
       [{:env {:registers {"foo" :r0}
               :for {}}}] s))))))

(def registers
  (merge (into {} (for [i (range 32)]
                    [(keyword (str "r" i)) (+ 0xe0 i)]))
         (into {} (for [i (range 32)]
                    [(keyword (str "r" i ".w0")) (+ 0x80 i)]))
         (into {} (for [i (range 32)]
                    [(keyword (str "r" i ".w1")) (+ 0xa0 i)]))
         (into {} (for [i (range 32)]
                    [(keyword (str "r" i ".b0")) i]))
         (into {} (for [i (range 32)]
                    [[:bo1 (keyword (str "r" i))] (+ 0x80 i)]))
         (into {} (for [i (range 32)]
                    [[:bo2 (keyword (str "r" i))] (+ 0x20 i)]))))


(defmulti instrunction-to-int :op)

(defmethod instrunction-to-int :ldi [{:keys [operand1 operand2] :as i}]
  (assert (contains? registers operand1) operand1)
  (assert (> 0x10000 operand2 -1) operand2)
  (let [r (get registers operand1)]
    (bit-or
     (bit-or
      (bit-or
       (bit-shift-left r 24)
       (bit-shift-left (bit-and operand2 0x00ff) 16))
      (bit-shift-left (bit-shift-right operand2 8) 8))
     0x24)))

(defmethod instrunction-to-int :sbbo [{:keys [operand1 operand2 operand3 operand4] :as i}]
  (assert (contains? registers [:bo1 operand1]))
  (assert (contains? registers [:bo2 operand2]))
  (assert (= operand4 4))
  (let [r1 (get registers [:bo1 operand1])
        r2 (get registers [:bo2 operand2])
        r3 (get registers [:bo2 operand3])]
    (bit-or (bit-or (bit-or (bit-shift-left r1 24)
                            (bit-shift-left r2 16))
                    (bit-shift-left operand3 8))
            0xe1)))

(def constants {:c24 0x38})

(defmethod instrunction-to-int :sbco [{:keys [operand1 operand2 operand3 operand4] :as i}]
  (assert (contains? registers [:bo1 operand1]) [:bo1 operand1])
  (assert (contains? constants operand2) operand2)
  (assert (contains? registers operand3))
  (assert (= 4 operand4))
  (let [r1 (get registers [:bo1 operand1])
        r2 (get constants operand2)
        r3 (get registers operand3)]
    (bit-or (bit-or (bit-or (bit-shift-left r1 24)
                            (bit-shift-left r2 16))
                    (bit-shift-left r3 8))
            0x80)))

(defmethod instrunction-to-int :nop0 [{:keys [operand1 operand2 operand3] :as i}]
  (unchecked-int 0xe0e1e2a0))

(defmethod instrunction-to-int :add [{:keys [operand1 operand2 operand3] :as i}]
  (assert (contains? registers operand1))
  (assert (contains? registers operand2))
  (assert (contains? registers operand3) operand3)
  (let [r1 (get registers operand1)
        r2 (get registers operand2)
        r3 (get registers operand3)]
    (bit-or (bit-or (bit-or (bit-shift-left r1 24)
                            (bit-shift-left r2 16))
                    (bit-shift-left r3 8))
            0x00)))

(defmethod instrunction-to-int :mov [{:keys [operand1 operand2] :as i}]
  (assert (contains? registers operand1))
  (assert (contains? registers operand2))
  (let [r1 (get registers operand1)
        r2 (get registers operand2)
        r3 (get registers operand2)]
    (bit-or (bit-or (bit-or (bit-shift-left r1 24)
                            (bit-shift-left r2 16))
                    (bit-shift-left r3 8))
            0x10)))

(defmethod instrunction-to-int :qblt [{:keys [operand1 operand2 operand3] :as i}]
  (assert (contains? registers operand2))
  (cond
   (and (number? operand3) (neg? operand1))
   (do
     (assert (> 256 operand3))
     (doto (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                                   (bit-shift-left (get registers operand2) 16))
                           (bit-shift-left operand3 8))
                   0x4f)
       (println)))
   :else (assert nil)))

(defmethod instrunction-to-int :qbgt [{:keys [operand1 operand2 operand3] :as i}]
  (assert (contains? registers operand2))
  (cond
   (and (number? operand3) (number? operand1) (neg? operand1))
   (do
     (assert (> 256 operand3))
     (doto (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                                   (bit-shift-left (get registers operand2) 16))
                           (bit-shift-left operand3 8))
                   0x67)
       (println)))
   :else (assert nil)))

(defmethod instrunction-to-int :qbne [{:keys [operand1 operand2 operand3] :as i}]
  (assert (contains? registers operand2))
  (if (and (number? operand3) (number? operand1) (neg? operand1))
    (do
     (assert (> 256 operand3))
     (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                             (bit-shift-left (get registers operand2) 16))
                     (bit-shift-left operand3 8))
             0x6f))
    (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                            (bit-shift-left (get registers operand2) 16))
                    (bit-shift-left (get registers operand3) 8))
            0x6e)))

(defmethod instrunction-to-int :qba [{:keys [operand1] :as i}]
  (bit-or (bit-shift-left operand1 24) 0x7f))

(defmethod instrunction-to-int :halt [_]
  (unchecked-int 0x0000002a))

(defn compile-basic [source]
  (let [ins (f (parse source))
        ins (map instrunction-to-int ins)
        baos (java.io.ByteArrayOutputStream. (* 4 (count ins)))
        out (java.io.DataOutputStream. baos)]
    (doseq [i ins]
      (.writeInt out (unchecked-int i)))
    (.close out)
    (.toByteArray baos)))

(comment
  (mapv instrunction-to-int
        (f
         (parse
          "
10 LET temp = 0x0
20 LET fib = 0x1
30 FOR number = 0x1 TO 0x10
40   LET pair = temp + fib
50   LET temp = fib
60   LET fib = pair
70 NEXT number
71 WRITE fib 0x1
80 END
")))




  (compile-basic
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
")

  (compile-basic
   "
10 LET fib = 0x1
11 LET a = 0x2
12 LET fib = fib + a
71 WRITE fib 0x0
80 END
")


  (compile-basic
   "
10 FOR number = 0x0 TO 0x10
40 NEXT number
50 WRITE number 0x0
60 END
")

    (compile-basic
     "
00 LET total = 0x0
01 LET temp = 0x0
02 LET offset = 0x0
03 LET eight = 0x8
10 FOR number = 0x0 TO 0x6
11   READ temp offset
12   LET total = total + temp
13   LET offset = offset + eight
40 NEXT number
50 WRITE number 0x0
60 END
")


        (compile-basic
     "
00 LET n = 0x0
01 LET one = 0x1
02 LET n = n + one
03 IF n = 0x5 THEN GOTO 50
04 GOTO 02
50 WRITE n 0x0
60 END
")


  )
