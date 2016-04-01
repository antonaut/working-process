;;;; links-graph - for visualization of .org-file headers

;;; This module visualizes an .org file in two ways. The first is
;;; through the org module, by using rhizome
;;; (https://github.com/ztellman/rhizome). The second is through Ardoq
;;; (https://ardoq.com) by using their client and REST-API.

;; We use links-graph.org so we can turn an .org-file into a tree and
;; manipulate it as we wish.

(ns links-graph.core
  (:require [links-graph.org :refer [children
                                     leaf-node?
                                     node-attr
                                     node-level
                                     node-type
                                     node?
                                     parse-org-file
                                     view-org-tree]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :refer [intersection]]
            [clojure.zip :as z]
            [ardoq.client :as client]))


;; Default contents of resources/private/env.json:
;;
;;  {
;;    "ARDOQ_API_URL"      : "https://app.ardoq.com"            ,
;;    "ARDOQ_API_TOKEN"    : "your-api-token" ,
;;    "ARDOQ_ORGANIZATION" : "ardoq"
;;  }

(defn new-client-from-env
  "Creates a new client from \"resources/private/env.json\"."
  []
  (let [ardoq-info (json/read-json (slurp (io/resource "private/env.json")))]
    (client/client {:url   (:ARDOQ_API_URL ardoq-info)
                    :token (:ARDOQ_API_TOKEN ardoq-info)
                    :org   (:ARDOQ_ORGANIZATION ardoq-info)})))


;; These components & references are defined in Ardoq. They were fetched by asking
;; the client for all the models and filtering them for "Org headers".

(def ardoq-component-types
  {:org-header "p1459500461531"
   :org-file   "p1459344429473"})

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

(defn org-node->ardoq-component
  [org-node workspace model]
  (let [component-name (node-attr org-node :title)
        component-type (node-type org-node)
        field-level    (node-level org-node)
        field-content  (node-attr org-node :content)]
    {:ardoq-component
     (client/->Component component-name
                         ""
                         (:_id workspace)
                         (:_id model)
                         (component-type ardoq-component-types)
                         (:content (node-attr org-node :content)))
     ;; TODO: create client/Fields of these
     :fields {:level   field-level
              :content field-content}}))

(defn ardoq-components->name-id-map [components]
  (into
   {}
   (map
    (fn [c] {(:name c) (:_id c)})
    components)))


(defn ardoq-components->ardoq-reference
  [workspace source target reftype]
  (client/->Reference (:_id workspace) source target reftype))


;;; In order to turn a tree of org-nodes into Ardoq entities and
;;; references, we traverse the tree and apply functions for every
;;; node and its children.


(defn org-tree-seq->ardoq-components
  [org-tree-seq workspace model]
  (mapv #(org-node->ardoq-component % workspace model) org-tree-seq))

(defn org-tree-map->ardoq-relations
  [org-tree-map workspace model])


(comment
  (let [client (new-client-from-env)]
    (->>
     (find-org-fields client)
     (filter #(field-on-component-types? % ardoq-component-types)))))


;;; IDEA: Use a zipper to traverse the tree and replace org-nodes with ardoq-entities.

;; Thanks for tree-edit, Alex Miller (alex@puredanger.com)
;; http://www.ibm.com/developerworks/library/j-treevisit/

(defn tree-edit
  "Edits the zipper nodes for which matcher returns true."
  [zipper matcher editor]
  (loop [loc zipper]
    (if (z/end? loc)
      (z/root loc)
      (if-let [matcher-result (matcher (z/node loc))]
        (recur (z/next (z/edit loc (partial editor matcher-result))))
        (recur (z/next loc))))))


(comment
  ;; Constuction and usage of a zipper
  (let [org-tree    (parse-org-file "./resources/test.org")
        tree-zipper (z/zipper node? children identity org-tree)]
    (->
     tree-zipper
     z/down
     z/down
     z/node
     (node-attr :title))))

(defn main- []
  (let [client      (new-client-from-env)
        model       (find-org-model client)
        workspace   (find-org-workspace client)
        ;;fields            (find-org-fields client)
        org-tree    (parse-org-file "./resources/test.org")
        tree-zipper (z/zipper node? children identity org-tree)
        org-zip-seq (loop [loc   tree-zipper
                           nodes ()]
                      (if (z/end? loc)
                        nodes
                        (let [node     (z/node loc)
                              next-loc (z/next loc)]
                          (if-not (nil? node)
                            (recur next-loc (cons node nodes))
                            (recur next-loc nodes)))))]
    (->>
     org-zip-seq)))

;; relations (org-tree-map->ardoq-relations org-tree-map)

(defn visualize-org-file
  "Visualizes a .org file"
  [filename]
  (view-org-tree (parse-org-file filename)))

;;(visualize-org-file "./resources/test.org")

(defn org-file->ardoq
  [filename]
  )
