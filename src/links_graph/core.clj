;;;; links-graph - for visualization of .org-file headers

;;; This module visualizes an .org file in two ways. The first is
;;; through the org module, by using rhizome
;;; (https://github.com/ztellman/rhizome). The second is through Ardoq
;;; (https://ardoq.com) by using their client and REST-API.

;; We use links-graph.org so we can turn an .org-file into a tree and
;; manipulate it as we wish.

(ns links-graph.core
  (:require [links-graph.org :refer :all :as org]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :refer [intersection]]
            [clojure.zip :as z]
            [ardoq.client :as client]))


;; Default contents of resources/private/env.json:
;;
;;  {
;;    "ARDOQ_API_URL"      : "https://app.ardoq.com",
;;    "ARDOQ_API_TOKEN"    : "your-api-token",
;;    "ARDOQ_ORGANIZATION" : "ardoq"
;;  }

(defn new-client-from-env
  "Creates a new client from \"resources/private/env.json\"."
  []
  (let [ardoq-info (json/read-json (slurp (io/resource "private/env.json")))]
    (client/client {:url   (:ARDOQ_API_URL ardoq-info)
                    :token (:ARDOQ_API_TOKEN ardoq-info)
                    :org   (:ARDOQ_ORGANIZATION ardoq-info)})))


;; These components & references are defined in Ardoq. They were
;; fetched by asking the client for all the models and filtering them
;; for "Org headers".

(def ardoq-component-types
  {:org-header-1 "p1459500461531"
   :org-header-2 "p1459500461531"
   :org-header-3 "p1459500461531"
   :org-header-4 "p1459500461531"
   :org-header-5 "p1459500461531"
   :org-header-6 "p1459500461531"
   :org-header-7 "p1459500461531"
   :org-file     "p1459344429473"})

(def ardoq-ref-types
  {:contains 4
   :implicit 2})

(defn find-org-model
  "Finds the models defined in Ardoq named \"Org headers\"."
  [client]
  (->> (client/find-all (client/map->Model {}) client)
       (filter #(= "Org headers" (:name %)))
       first))

(defn find-org-workspace
  "Finds a workspace named \"orgfiles\" in Ardoq."
  [client]
  (->> (client/find-all (client/map->Workspace {}) client)
       (filter #(= "orgfiles" (:name %)))
       first))

(defn find-all-fields
  [client]
  ;; QUESTION: How to query? Ex. all fields used in a certain workspace
  ;; ANSWER: Filter in client.
  (->> (client/find-all (client/map->Field {}) client)))

(defn field-on-component-types?
  "Is this client/Field defined for given component-types?"
  [field ardoq-component-types]
  (let [component-types (set (:componentType field))
        org-components  (set (vals ardoq-component-types))]
    ((complement zero?) (count (intersection component-types org-components)))))

(defn add-ardoq-component
  "Adds Ardoq components to a map-node."
  [node workspace model]
  (if-not (nil? node)
    (let [org-node       (:org-node node)
          component-name (node-attr org-node :title)
          component-type (node-type org-node)
          field-level    (node-level org-node)
          field-content  (node-attr org-node :content)]
      (assoc
       node
       :ardoq-component
       (client/->Component component-name
                           ""
                           (:_id workspace)
                           (:_id model)
                           (component-type ardoq-component-types)
                           (:content (node-attr org-node :content)))
       :fields {:level   field-level
                :content field-content}))))


(comment
  (let [client (new-client-from-env)]
    (->>
     (find-org-fields client)
     (filter #(field-on-component-types? % ardoq-component-types)))))

(defn create-ardoq-components
  [workspace model  map-tree graph]
  (loop [components {}
         nodes      (keys graph)
         current    (first nodes)
         ]
    (if (empty? nodes)
      components
      (let [component       (-> (add-ardoq-component
                                 (find-by-id map-tree current)
                                 workspace
                                 model)
                                (dissoc :children :org-node))
            ardoq-component (:ardoq-component component)]
        (recur (assoc components (:id component)
                      (conj ardoq-component
                            (dissoc component :ardoq-component)))
               (rest nodes)
               (first (rest nodes)))))))


(defn find-component
  [src components]
  (loop [comps   components
         current (first comps)]
    (cond (= src (keyword (:id current))) current
          (empty? comps)                  nil
          :else                           (recur (rest comps) (first (rest comps))))))

(defn create-reference
  [workspace source target reftype components]
  (let [src (find-component source components)
        tar (find-component target components)]
    (assoc (client/->Reference (:_id workspace) (:_id src) (:_id tar))
           :type reftype)))


(defn create-ardoq-references
  [graph components workspace]
  (loop [nodes          (keys graph)
         current-source (first nodes)
         references     {}]
    (if (empty? nodes)
      references
      (recur (rest nodes)
             (first (rest nodes))
             (assoc references
                    current-source
                    (mapv #(create-reference
                            workspace
                            current-source
                            %
                            (:contains ardoq-ref-types)
                            components)
                          (current-source graph)))))))

(defn components->ardoq
  [components client]
  (let [comps           (group-by #(= (:type %) :org-file)
                                  components)
        file            (first (get comps true))
        headers         (get comps false)
        file-created    (client/create file client)
        file-_id        (:_id file-created)
        headers-created (mapv #(client/create
                                (assoc % :parent file-_id)
                                client) headers)]
    (conj headers-created file-created)))

(defn main- []
  (let [client                   (new-client-from-env)
        model                    (find-org-model client)
        workspace                (find-org-workspace client)
        ;;fields            (find-org-fields client)
        org-tree                 (parse-file "./resources/test.org")
        map-tree                 (org-tree->map-tree org-tree)
        graph                    (map-tree->graph map-tree)
        ardoq-components         (create-ardoq-components
                                  workspace
                                  model
                                  map-tree
                                  graph)
        created-ardoq-components (components->ardoq
                                  (vals ardoq-components)
                                  client)]
    (mapv #(client/create % client)
          (->
           (create-ardoq-references graph created-ardoq-components workspace)
           vals
           flatten
           ))))

;; relations (org-tree-map->ardoq-relations org-tree-map)

(defn visualize-org-file
  "Visualizes a .org file"
  [filename]
  (view-tree (parse-file filename)))

;;(visualize-org-file "./resources/test.org")

(defn org-file->ardoq
  [filename])
