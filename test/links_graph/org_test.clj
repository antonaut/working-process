(ns links-graph.org-test
  (:require [clojure.test :refer :all]
            [links-graph.org :refer :all]))

;; Small testcase
(def t1
  [:org-file
   [:org-header-1 [:title "header111"]
    [:org-header-2 [:title "Header2"]]]
   [:org-header-1 ]
   [:title "t1"]])


;; Node titles

(node-title [:org-header-2
             [:title "header22"]])

(remove #(or (empty? %) (nil? %))
        (map node-title (children t1)))


;; Slightly bigger test case
(def testorg (parse-org-resource "test.org"))


;; Get all content
(let  [org-tree-seq (tree-seq node? children testorg)]
  (->> org-tree-seq (map #(node-attr % :content))
       flatten
       (filter some?)))

;; First level
(->> testorg
     children)

;; 2nd level headers
(->> testorg
     children
     (mapcat children))

;;(def spec-org (parse-org-file spec-file-path))

(view-org-tree t1)
