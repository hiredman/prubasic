(ns prubasic.core
  (:require [prubasic.parser :refer [parse]]))

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

(defn ldi [target-register constant env & [label]]
  {:op :ldi
   :operand1 target-register
   :operand2 constant
   :env env
   :label label
   :reads #{}
   :writes #{target-register}})

(defn add [target-register source-one source-two env & [label]]
  {:op :add
   :operand1 target-register
   :operand2 source-one
   :operand3 source-two
   :env env
   :label label
   :reads #{source-one source-two}
   :writes #{target-register}})

(defn mov [target-register source-register env & [label]]
  {:op :mov
   :operand1 target-register
   :operand2 source-register
   :env env
   :label label
   :reads #{source-register}
   :writes #{target-register}})

(defn qbgt [target-label test-register constant env & [label]]
  {:op :qbgt
   :operand1 target-label
   :operand2 test-register
   :operand3 constant
   :env env
   :label label
   :writes #{}
   :reads (if (number? constant)
            #{test-register}
            #{test-register constant})})

(defn halt [env & [label]]
  {:op :halt
   :env env
   :label label
   :reads #{}
   :writes #{}})

(defn nop0 [env & [label]]
  {:op :nop0
   :operand1 :r1
   :operand2 :r1
   :operand3 :r2
   :env env
   :label label
   :reads #{}
   :writes #{}})

(defn sbco [source constant offset width env & [label]]
  {:op :sbco
   :operand1 source
   :operand2 constant
   :operand3 offset
   :operand4 width
   :label label
   :env env
   :reads #{source offset}
   :writes #{}})

(defn qbne [label test-register test env & [label]]
  {:op :qbne
   :operand1 label
   :operand2 test-register
   :operand3 test
   :label label
   :env env
   :reads (if (number? test)
            #{test-register}
            #{test-register test})
   :writes #{}})

(defn qbge [label test-register test env & [label]]
  {:op :qbge
   :operand1 label
   :operand2 test-register
   :operand3 test
   :label label
   :env env
   :reads (if (number? test)
            #{test-register}
            #{test-register test})
   :writes #{}})

(defn expression-rewrite [env label target-r x]
  (loop [to-push (seq (expression-to-stack env x))
         stack []
         label label
         result []]
    (if (empty? to-push)
      (if (keyword? (peek stack))
        (conj result (mov target-r (peek stack) env))
        (conj result (ldi target-r (peek stack) env)))
      (let [[x & xs] to-push]
        (case x
          :+ (let [a (peek stack)
                   stack (pop stack)
                   b (peek stack)
                   stack (pop stack)
                   stack (conj stack :r2)]
               (recur xs stack nil (into result
                                         (cond
                                          (and (keyword? a) (keyword? b)) [(add :r2 a b env)]
                                          (keyword? a) [(ldi :r2 b env) (add :r2 :r2 a env)]
                                          (keyword? b) [(ldi :r2 a env) (add :r2 :r2 b env)]
                                          :else [(ldi :r2 a env) (ldi :r1 b env) (add :r2 :r2 :r1 env)]))))
          (recur xs (conj stack x) nil result))))))

(defn find-register [env name]
  (let [r (if (contains? (:registers env)  name)
            (get-in env [:registers name])
            (keyword (str "r" (+ 3 (count (:registers env))))))
        env (assoc-in env [:registers name] r)]
    [r env]))

(defmethod pass1 :let [env [_ [_ label] [_ [_ variable-name] [thing-type :as thing]] :as command-line]]
  (let [[r new-env] (find-register env variable-name)]
    (assoc-in (expression-rewrite new-env label r thing) [0 :label] label)))

(defmethod pass1 :for [env [_ [_ label] [_ [_ variable-name] [thing-type :as thing] [_ [_ _ [_ digits]]]]]]
  (case thing-type
    :value (let [[_ [_ _ [_ thing-digits]]] thing
                 thing (Long/parseLong thing-digits 16)
                 limit (Long/parseLong digits 16)
                 next-target (gensym 'for)
                 end (gensym 'forend)
                 [r new-env] (find-register env variable-name)
                 new-env (assoc-in new-env [:for r :target] next-target)
                 new-env (assoc-in new-env [:for r :limit] limit)]
             [(ldi r thing new-env label)
              ;; nop
              (mov :r1 :r1 new-env next-target)])))

(defmethod pass1 :next [env [_ [_ label] [_ [_ vn]]]]
  (assert (contains? (:registers env) vn) vn)
  [(ldi :r1 1 env label)
   (add (get-in env [:registers vn]) (get-in env [:registers vn]) :r2 env)
   (qbgt (get-in env [:for (get-in env [:registers vn]) :target])
         (get-in env [:registers vn])
         (get-in env [:for (get-in env [:registers vn]) :limit])
         env)])

(def PRU0-ARM-INTERRUPT 19)

(defmethod pass1 :end [env [_ [_ label]]]
  [(mov :r1 :r1 env)
   (ldi :r31.b0 (+ PRU0-ARM-INTERRUPT 16) env)
   (halt env)])

(defmethod pass1 :write [env [_ [_ label] [_ [_ vn] exp]]]
  (let [source (get-in env [:registers vn])]
    (vec
     (concat
      [(nop0 env label)]
      (expression-rewrite env nil :r2 exp)
      [(sbco source :c24 :r2 0x4 env)]))))

(defmethod pass1 :read [env [_ [_ label] [_ [_ vn] exp]]]
  (let [[dest new-env] (find-register env vn)]
    (vec
     (concat
      [(nop0 new-env label)]
      (expression-rewrite new-env nil :r2 exp)
      [{:op :lbco
        :operand1 dest
        :operand2 :c24
        :operand3 :r2
        :operand4 0x4
        :label nil
        :env new-env}]))))

;; TODO: expressionize using :r1 and :r2
(defmethod pass1 :if [env [_ [_ label] [_ [_ a op b] thing]]]
  (case [(second op) (first (second a)) (first (second b))]
    ["=" :variable-name :value] (let [r (get-in env [:registers (second (second a))])
                                      neq (gensym 'neq)
                                      const (Long/parseLong
                                             (-> b second second last second)
                                             16)]
                                  (concat [(ldi :r2 const env label)
                                           (qbne neq r :r2 env)]
                                          (pass1 env [nil [_ nil] thing])
                                          [(nop0 env neq)]))
    ["<" :variable-name :value] (let [r (get-in env [:registers (second (second a))])
                                      neq (gensym 'neq)
                                      const (Long/parseLong
                                             (-> b second second last second)
                                             16)]
                                  (concat [(ldi :r2 const env label)
                                           (qbge neq r :r2 env)]
                                          (pass1 env [nil [_ nil] thing])
                                          [(nop0 env neq)]))))

(defmethod pass1 :goto [env [_ [_ label] [_ [_ target-label]]]]
  [{:op :qba
    :operand1 target-label
    :env env
    :label label}])

(defn resolve-labels [s]
  (let [idx (group-by :label s)]
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
        instr))))

(defn f [s]
  (resolve-labels
   (keep-indexed
    #(assoc %2 :n %1)
    (concat
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
       :env {}}]
     (rest
      (reduce
       (fn [result command-line]
         (let [e (:env (peek result))]
           (into result (pass1 e command-line))))
       [{:env {:registers {} :for {}}}] s))))))

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

(defmethod instrunction-to-int :lbco [{:keys [operand1 operand2 operand3 operand4] :as i}]
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
            0x90)))

(defmethod instrunction-to-int :nop0 [{:keys [operand1 operand2 operand3] :as i}]
  (unchecked-int 0xe0e1e2a0))

(defmethod instrunction-to-int :add [{:keys [operand1 operand2 operand3] :as i}]
  (assert (contains? registers operand1))
  (assert (contains? registers operand2) operand2)
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

(defmethod instrunction-to-int :qbge [{:keys [operand1 operand2 operand3] :as i}]
  (if (and (number? operand3) (number? operand1) (neg? operand1))
    (do
      (assert nil)
      (assert (> 256 operand3))
      (doto (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                                    (bit-shift-left (get registers operand2) 16))
                            (bit-shift-left operand3 8))
                    0x76)
        (println)))
    (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                            (bit-shift-left (get registers operand2) 16))
                    (bit-shift-left (get registers operand3) 8))
            0x76)))

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


  (compile-basic
   "
00 READ n 0x0
10 LET temp = 0x0
20 LET fib = 0x1
30 FOR number = 0x1 TO n
40   LET pair = temp + fib
50   LET temp = fib
60   LET fib = pair
70 NEXT number
71 WRITE fib 0x0
80 END
")


  (compile-basic
   "
00 READ n 0x0
10 LET temp = 0x0
20 LET fib = 0x1
30 LET number = 0x1
40 LET pair = temp + fib
50 LET temp = fib
60 LET fib = pair
70 LET number = number + 0x1
80 IF number < 0x16 THEN GOTO 40
71 WRITE fib 0x0
80 END
")


  (compile-basic
   "
10 LET one = 0x1
20 LET two = 0x2
30 IF one < 0x2 THEN WRITE one 0x0
40 IF two < 0x1 THEN WRITE two 0x0
50 END
")
  )
