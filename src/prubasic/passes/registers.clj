(ns prubasic.passes.registers)

(def registers (set (for [i (range 1 31)] (keyword (str "r" i)))))

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

(defmethod allocate-registers-for-instruction :jmp [registers allocations instr]
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
