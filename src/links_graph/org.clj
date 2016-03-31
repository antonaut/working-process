(ns links-graph.org
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [rhizome.viz :as viz]))

(defn parse-org-file
  [filename]
  (let [org-parser (insta/parser (io/resource "org.bnf"))
        org-file   (io/file filename)]
    (conj (org-parser (slurp org-file))
          [:title filename])))

(defn parse-org-resource
  [resource-filename]
  (parse-org-file
   (io/resource resource-filename)))

(defn node-type
  [node]
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
        type))))

(defn node?
  [maybe-node]
  (some? (node-type maybe-node)))

(defn node-attr
  [node attr]
  (when (node? node)
    (->> node
         (filter sequential?)
         (remove empty?)
         (filter #(= (first %) attr))
         (map (comp first rest))
         first)))


(defn node-title
  [node]
  (node-attr node :title))



(defn children
  [node]
  (vec (filter node? node)))


(defn view-org-tree
  [org-tree]
  (viz/view-tree node? children org-tree
                 :node->descriptor (fn [node] {:label (node-title node)})))
