;;;; # orgviz - for visualization of .org-file headers

;;; This module visualizes an .org file in two ways. The first is
;;; through the org module, by using
;;; [rhizome](https://github.com/ztellman/rhizome). The second is
;;; through [Ardoq](https://ardoq.com) by using their client and
;;; REST-API.

;; We use links-graph.org so we can turn an .org-file into a tree and
;; manipulate it as we wish.

(ns links-graph.orgviz
  (:refer-clojure :exclude [update])
  (:require [links-graph.org :refer :all :as org]
            [links-graph.ardoq-helper :refer :all :as ardoq]
            [clojure.set :refer [intersection]]
            [clojure.zip :as z]
            [ardoq.client :as client]))

;; ## Ardoq types
;; These components & references are defined in Ardoq. They were
;; fetched by asking the client for all the models and filtering them
;; for "Org headers".

(def ardoq-component-types
  {:org-header-1 "p1464792496575"
   :org-header-2 "p1464792496575"
   :org-header-3 "p1464792496575"
   :org-header-4 "p1464792496575"
   :org-header-5 "p1464792496575"
   :org-header-6 "p1464792496575"
   :org-header-7 "p1464792496575"
   :org-file     "p1463698041173"})

(def ardoq-ref-types
  {:contains 4
   :implicit 2})


(defn find-org-model
  "Finds the models defined in Ardoq named \"Org headers\"."
  [client]
  (ardoq/find-model-by-name client "Org headers"))


(defn find-org-workspace
  "Finds a workspace named \"orgfiles\" in Ardoq."
  [client]
  (ardoq/find-workspace-by-name client "orgfiles"))


(defn field-on-component-types?
  "Is this client/Field defined for given component-types?"
  [field ardoq-component-types]
  (let [component-types (set (:componentType field))
        org-components (set (vals ardoq-component-types))]
    ((complement zero?) (count (intersection component-types org-components)))))

(defn add-ardoq-component
  "Adds Ardoq components to a map-node."
  [node workspace model]
  (if-not (nil? node)
    (let [org-node (:org-node node)
          component-name (node-attr org-node :title)
          component-type (node-type org-node)
          field-level (node-level org-node)
          field-content (node-attr org-node :content)]
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

;; TODO: Work on fields
(comment
  (let [client (ardoq/new-client-from-env)])
  (filter #(field-on-component-types? % ardoq-component-types) (find-org-fields client)))


(defn create-ardoq-components
  "Creates Ardoq components from a map-tree and a graph."
  [workspace model map-tree graph]
  (loop [components {}
         nodes (keys graph)
         current (first nodes)]
    (if (empty? nodes)
      components
      (let [component (dissoc (add-ardoq-component
                                (find-by-id map-tree current)
                                workspace
                                model) :children :org-node)
            ardoq-component (:ardoq-component component)]
        (recur (assoc components (:id component)
                                 (conj ardoq-component
                                       (dissoc component :ardoq-component)))
               (rest nodes)
               (first (rest nodes)))))))


(defn find-component
  "Performs a linear search in _components_ for the component with id
  _source-id_. Returns nil if a component isn't found."
  [source-id components]
  (loop [comps components
         current (first comps)]
    (cond (= source-id (keyword (:id current))) current
          (empty? comps) nil
          :else (recur (rest comps) (first (rest comps))))))

;;; ### References

(defn create-reference
  [workspace source target reftype components]
  (let [src (find-component source components)
        tar (find-component target components)]
    (assoc (client/->Reference (:_id workspace) (:_id src) (:_id tar))
      :type reftype)))


(defn create-ardoq-references
  [graph components workspace]
  (loop [nodes (keys graph)
         current-source (first nodes)
         references {}]
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
  (let [comps (group-by #(= (:type %) :org-file)
                        components)
        file (first (get comps true))
        headers (get comps false)
        file-created (client/create file client)
        file-_id (:_id file-created)
        headers-created (mapv #(client/create
                                (assoc % :parent file-_id)
                                client) headers)]
    (conj headers-created file-created)))

(defn references->ardoq
  [components graph client workspace]
  (mapv #(client/create % client)
        (->
          (create-ardoq-references graph components workspace)
          vals
          flatten)))

(comment
  (let [filename "./resources/test.org"
        client (ardoq/new-client-from-env)
        model (find-org-model client)
        workspace (find-org-workspace client)
        ;;fields            (find-org-fields client)
        org-tree (parse-file filename)
        map-tree (org-tree->map-tree org-tree)
        graph (map-tree->graph map-tree)
        ardoq-components (create-ardoq-components
                           workspace
                           model
                           map-tree
                           graph)] ardoq-components))


(defn org-file->ardoq
  "Adds a .org-file to Ardoq.
  Returns a map with the references and the components created."
  [filename]
  (let [client (ardoq/new-client-from-env)
        model (find-org-model client)
        workspace (find-org-workspace client)
        ;;fields            (find-org-fields client)
        org-tree (parse-file filename)
        map-tree (org-tree->map-tree org-tree)
        graph (map-tree->graph map-tree)
        ardoq-components (create-ardoq-components
                           workspace
                           model
                           map-tree
                           graph)
        created-ardoq-components (components->ardoq
                                   (vals ardoq-components)
                                   client)
        created-ardoq-references (references->ardoq
                                   created-ardoq-components
                                   graph
                                   client
                                   workspace)]
    {:ardoq-references created-ardoq-references
     :ardoq-components created-ardoq-components}))


(defn visualize-org-file
  "Visualizes a .org file with links-graph.org/view-tree."
  [filename]
  (view-tree (parse-file filename)))



;; <pre>(visualize-org-file "./resources/test.org")</pre>

;; (links-graph.org/parse-file "/home/aerholt/Dropbox/thesis/specification/specification.org")
