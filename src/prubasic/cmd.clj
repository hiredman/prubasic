(ns prubasic.cmd
  (:require [prubasic.core :refer [compile-basic]]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main [& args]
  (let [src (slurp *in*)
        code (compile-basic src)]
    (io/copy code *out*)))


