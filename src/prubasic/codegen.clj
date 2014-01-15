(ns prubasic.codegen)

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
  (assert (contains? registers operand1) i)
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
     (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                             (bit-shift-left (get registers operand2) 16))
                     (bit-shift-left operand3 8))
             0x4f))
   :else (assert nil)))

(defmethod instrunction-to-int :qbgt [{:keys [operand1 operand2 operand3] :as i}]
  (assert (contains? registers operand2))
  (if (and (number? operand3) (number? operand1) (neg? operand1))
   (do
     (assert (> 256 operand3))
     (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                             (bit-shift-left (get registers operand2) 16))
                     (bit-shift-left operand3 8))
             0x67))
   (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                           (bit-shift-left (get registers operand2) 16))
                   (bit-shift-left (get registers operand3) 8))
           0x60)))

(defmethod instrunction-to-int :qbge [{:keys [operand1 operand2 operand3] :as i}]
  (if (and (number? operand3) (number? operand1) (neg? operand1))
    (do
      (assert nil)
      (assert (> 256 operand3))
      (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                              (bit-shift-left (get registers operand2) 16))
                      (bit-shift-left operand3 8))
              0x76))
    (bit-or (bit-or (bit-or (bit-shift-left operand1 24)
                            (bit-shift-left (get registers operand2) 16))
                    (bit-shift-left (get registers operand3) 8))
            0x70)))

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

(defmethod instrunction-to-int :jmp [{:keys [operand1] :as i}]
  (bit-or (bit-shift-left operand1 16) 0x12))

(defmethod instrunction-to-int :halt [_]
  (unchecked-int 0x0000002a))

(defn code-gen [ast]
  (let [baos (java.io.ByteArrayOutputStream. (* 4 (count ast)))
        out (java.io.DataOutputStream. baos)]
    (doseq [i ast]
      (.writeInt out (unchecked-int (instrunction-to-int i))))
    (.close out)
    (.toByteArray baos)))
