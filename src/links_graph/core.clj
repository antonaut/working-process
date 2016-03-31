;; This is a small application for showing the headlines of an .org-file.

(ns links-graph.core
  (:require [links-graph.org :refer [view-org-tree
                                     parse-org-file
                                     node?
                                     node-attr
                                     children]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ardoq.client :as client]))


(def ardoq-info (json/read-json (slurp (io/resource "private/env.json"))))

(def ardoq-model-component-type
  {:org-header "p1459344429473"})

(defn find-org-model
  [client]
  (->> (client/find-all (client/map->Model {}) client)
       (filter #(= "Org headers" (:name %)))
       first))

(defn find-org-workspace
  [client]
  (->> (client/find-all (client/map->Workspace {}) client)
       (filter #(= "orgfiles" (:name %)))
       first))

(defn org-node->header-component
  [org-node workspace model]
  (let [component-name (node-attr org-node :title)]
    (conj org-node [:ardoq-component
                    (client/->Component component-name
                                        ""
                                        (:_id workspace)
                                        (:_id model)
                                        (:org-header ardoq-model-component-type)
                                        (:content (node-attr org-node :content)))])))

(defn org-tree-seq->ardoq-header-components
  [org-tree-seq workspace model]
  (mapv #(org-node->header-component % workspace model) org-tree-seq))

(defn org-tree-map->ardoq-relations
  [org-tree-map workspace model])

(defn main- []
  (let [client            (client/client {:url   (:ARDOQ_API_URL ardoq-info)
                                          :token (:ARDOQ_API_TOKEN ardoq-info)
                                          :org   (:ARDOQ_ORGANIZATION ardoq-info)})
        model             (find-org-model client)
        workspace         (find-org-workspace client)
        org-tree          (parse-org-file "./resources/test.org")
        org-tree-seq      (tree-seq node? children org-tree)
        header-components (org-tree-seq->ardoq-header-components
                           org-tree-seq
                           workspace model)
        org-tree-map      (into {} (for [node org-tree-seq]
                                     [node (children node)]))
        ;;        relations         (org-tree-map->ardoq-relations org-tree-map)
        ]
    (->> header-components
         (map #(node-attr % :ardoq-component))
         (map  #(client/create % client)))))


;; Visualizes a .org file
(defn visualize-org-file
  [filename]
  (view-org-tree (parse-org-file filename)))

(visualize-org-file "./resources/test.org")

(defn org-file->ardoq
  [filename] )
