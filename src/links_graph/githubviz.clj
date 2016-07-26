(ns links-graph.githubviz
;;  (:refer-clojure :exclude [update])
  (:require [links-graph.github :refer :all :as github]
            [links-graph.ardoq-helper :as ardoq-helper]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [ardoq.client :as client :exclude update]))

(defn find-github-model
  [client]
  (ardoq-helper/find-model-by-name client "github-repo-mini-model"))


(defn find-github-workspace
  "Finds the github workspace in Ardoq."
  [client]
  (ardoq-helper/find-workspace-by-name client "github"))

(def github-model-data
  (->
   "github-model.edn"
   io/resource
   io/reader
   java.io.PushbackReader.
   edn/read))

(defn pre-order [tree f children]
  (f tree)
  (map (fn [[k v] & _]
         (pre-order {k v} f children))
       (children tree))
  nil)

(defn node-id [node]
  (first (keys node)))

(defn print-node
  [node]
  (let [id (node-id node)]
    (print
     {:id id
      :name (:name (id node))})))

(defn minimal-node
  [node]
  (let [id (node-id node)]
    {:name (:name (id node))}))


;; (print-node (:root github-model-data))

(defn node-children [node]
  (:children ((node-id node) node)))

(def github-model (atom {}))

(pre-order
 (:root github-model-data)
 (fn [node]
   (swap! github-model #(assoc % (node-id node) (minimal-node node))))
 node-children)

(apply merge
 (mapv
  (fn [me]
    {(keyword (:name (second me))) (name (first me))})
      @github-model))

;; @github-model

;; (keys (node-children (:root github-model-data)))

;; (pre-order
;;  (:root github-model-data)
;;  print-node
;;  node-children)
