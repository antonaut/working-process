(ns links-graph.org
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [rhizome.viz :as viz]
            [com.rpl.specter :as specter]]))

(defn parse-org-file [filename]
  (let [org-parser (insta/parser (io/resource "org.bnf"))
        org-file (io/file filename)]
    (conj (org-parser (slurp org-file))
          [:title filename])))

(defn parse-org-resource [resource-filename]
  (parse-org-file (io/resource resource-filename)))

(defn node-type [node]
  (when (sequential? node)
    (let [type (first node)]
      (when
          (and (keyword? type)
               (contains? #{:org-file
                            :org-header-1
                            :org-header-2
                            :org-header-3
                            :org-header-4
                            :org-header-5
                            :org-header-6
                            :org-header-7} type))
        type))))n

(defn node? [maybe-node]
  (some? (node-type maybe-node)))

(defn node-title [node]
  (when (node? node)
    (->> node
         (filter sequential?)
         (remove empty?)
         (filter #(= (first %) :title))
         (map (comp first rest)))))

(defn children [node]
  (vec (filter node? node)))


(defn view-org-tree [org-tree]
  (viz/view-tree node? children org-tree
                 :node->descriptor (fn [node] {:label (node-title node)})))

;; TESTS

(def t1 [:org-file
         [:org-header-1 [:title "header111"]
          [:org-header-2 [:title "Header2"]]]
         [:org-header-1 ]
         [:title "t1"]])


;; Node titles

(node-title [:org-header-2 [:title "header22"]])
(remove #(or (empty? %) (nil? %))
        (map node-title (children t1)))


;; Slightly bigger test case
(def testorg (parse-org-resource "test.org"))

;; First level
(->> testorg
     children)

;; 2nd level headers
(->> testorg
     children
     (mapcat children))

;;(def spec-org (parse-org-file spec-file-path))

(view-org-tree t1)
