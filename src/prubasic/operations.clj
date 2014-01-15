(ns prubasic.operations)

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

(defn qblt [target-label test-register constant env & [label]]
  {:op :qblt
   :operand1 target-label
   :operand2 test-register
   :operand3 constant
   :env env
   :label label
   :writes #{}
   :reads (if (number? constant)
            #{test-register}
            #{test-register constant})})

(defn jmp [target-label env & [label]]
  {:op :jmp
   :operand1 target-label
   :env env
   :label label
   :writes #{}
   :reads #{}})

(defn qbeq [target-label test-register constant env & [label]]
  {:op :qbeq
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

(defn qbne [target-label test-register test env & [label]]
  {:op :qbne
   :operand1 target-label
   :operand2 test-register
   :operand3 test
   :label label
   :env env
   :reads (if (number? test)
            #{test-register}
            #{test-register test})
   :writes #{}})

(defn qbge [target-label test-register test env & [label]]
  {:op :qbge
   :operand1 target-label
   :operand2 test-register
   :operand3 test
   :label label
   :env env
   :reads (if (number? test)
            #{test-register}
            #{test-register test})
   :writes #{}})
