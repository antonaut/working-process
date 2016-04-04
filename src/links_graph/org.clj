(ns links-graph.org
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [rhizome.viz :as viz]
            [clojure.set :as set]))

(defn parse-string
  [string]
  (let [org-parser (insta/parser (io/resource "org.bnf"))]
    (org-parser string)))

(defn parse-file
  [filename]
  (let [org-file (io/file filename)
        org-tree (parse-string (slurp org-file))]
    (conj org-tree
          [:title filename])))


(defn parse-resource
  [resource-filename]
  (parse-file
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

(defn children
  [node]
  (vec (filter node? node)))


(defn leaf-node?
  [node]
  (-> node
      children
      count
      zero?))

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

(defn node-level
  [node]
  (let [type-str (name (node-type node))]
    (if (not (.contains type-str "header"))
      0
      (Character/getNumericValue (last (seq type-str))))))

(defn node->map
  [node]
  (when (node? node)
    (let [type     (node-type node)
          id       (keyword (gensym "node"))
          org-node node]
      {:type     type
       :id       id
       :org-node node
       })))

(defn org-tree->map-tree
  [root]
  (when (not (nil? root))
    (let [node     (node->map root)
          children (mapv org-tree->map-tree (children root))]
      (assoc node :children children))))

(defn map-node->parent-child-ids
  [root]
  (when root
    (let [child-ids (mapv :id (:children root))]
      (assoc {} (:id root) child-ids))))

(defn map-tree->graph
  "Turns a org-map-tree into a
  ;=>  {:node-id [:child-1-id :child-2-id ...]  ...}
  -representation of a graph by using
  a BFS-traversal on the nodes."
  [start]
  (let [visited #{} ; Don't really needed since it's a tree, but whatever.
        queue   (conj clojure.lang.PersistentQueue/EMPTY start)
        ]
    (loop [visited visited
           current (peek queue)
           queue   queue
           g       {}]
      (if (zero? (count queue))
        g
        (if-not (contains? visited (:id current))
          (recur (conj visited current)
                 (peek queue)
                 (into (pop queue) (:children current))
                 (conj g (map-node->parent-child-ids current)))
          (recur visited
                 (peek queue)
                 (into (pop queue) (:children current))
                 g))))))

(defn find-by-id
  [map-tree id]
  (when map-tree
    (if (= id (:id map-tree))
      map-tree
      (first (remove nil? (map #(find-by-id % id) (:children map-tree)))))))


(defn view-tree
  "Visualizes an org-tree"
  [org-tree]
  (viz/view-tree node? children org-tree
                 :node->descriptor (fn [node] {:label (node-title node)})))
