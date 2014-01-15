(ns prubasic.core
  (:require [prubasic.compiler :as c]))

;; http://glind.customer.netspace.net.au/gwbas-17.html

(defn compile-basic [src]
  (c/compile-basic src))
