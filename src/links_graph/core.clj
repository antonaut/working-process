;; This is a small application for showing the headlines of an .org
;; file like a graph (mind map).

(ns links-graph.core
  (:require [links-graph.org :as org]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ardoq.client :as client]))


(def ardoq-info (json/read-json (slurp (io/resource "private/env.json"))))

;; Visualizes a .org file
(defn visualize-org-file
  [filename]
  (org/view-org-tree (org/parse-org-file filename)))

(defn org-file->ardoq
  [filename]
  (let [client (client/client {:url   (:ARDOQ_API_URL ardoq-info)
                               :token (:ARDOQ_API_TOKEN ardoq-info)
                               :org   (:ARDOQ_ORGANIZATION ardoq-info)})]
    (client/find-all (client/map->Model {}) client)))


(defn -main [])
