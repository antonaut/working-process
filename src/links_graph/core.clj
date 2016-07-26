(ns links-graph.core
  (:require [links-graph.orgviz :refer :all]))

(defn main- []
  (org-file->ardoq "./resources/test.org"))
