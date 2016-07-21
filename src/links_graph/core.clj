(ns links-graph.core
  (:require [links-graph.orgviz]))

(defn main- []
  (orgviz/org-file->ardoq "./resources/test.org"))
