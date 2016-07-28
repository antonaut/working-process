(ns links-graph.githubviz
  (:refer-clojure :exclude [update])
  (:require [links-graph.github :refer :all :as github]
            [links-graph.ardoq-helper :as ardoq-helper]
            [camel-snake-kebab.core :refer :all]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [ardoq.client :as client])
  (:import (org.apache.commons.lang3 StringEscapeUtils)))

(def ardoq-workspace-id "5721ee1272fa6d3b497b82ee")
(def ardoq-model-id "5799fdf29f2a262b179d315f")

(defn find-github-model
  [c]
  (ardoq-helper/find-model-by-name c "github-repo-mini-model"))

(defn find-github-workspace
  "Finds the github workspace in Ardoq."
  [client]
  (ardoq-helper/find-workspace-by-name client "Github Issues"))

(comment
  ;; This fetches model data from Ardoq into a local resource
  (let [c (ardoq-helper/new-client-from-env)
        github-model (find-github-model c)]
    (with-open [wr (io/writer (io/resource "github-model.edn"))]
      (.write wr (pr-str github-model)))))

(def github-model-data
  (->
   "github-model.edn"
   io/resource
   io/reader
   java.io.PushbackReader.
   (#(edn/read {:default hash-map} %))
   first
   val))

;; Tree functions

(defn pre-order
  "Traverses tree, applies f to each node before traversing the
  children of the node."
  [tree f children]
  [(f tree)
   (map (fn [[k v]]
          (pre-order {k v} f children))
        (children tree))])

(defn node-id [node]
  (first (keys node)))

(defn print-node
  [node]
  (let [id (node-id node)]
    (print
     {:id id
      :name
      (:name (id node))})))

(defn minimal-node
  [node]
  (let [id (node-id node)]
    {:id id
     :name (->kebab-case
            (:name (id node)))}))

;; (print-node (:root github-model-data))

(defn node-children [node]
  (:children ((node-id node) node)))


;; Components & References

(defn github-component-types []
  (apply merge
         (map #(hash-map (keyword (:name %)) (name (:id %)) )
              (flatten
               (pre-order
                (:root github-model-data)
                minimal-node
                node-children)))))

(defn github-reference-types []
  (apply
   merge
   (mapv
    (fn [[k v]] (let [kwstr (->kebab-case (:name v))
                      kw (keyword (if (empty? kwstr) "default" kwstr))
                      val (Integer/parseInt (clojure.string/lower-case (name k)))] (hash-map kw val)))
    (:referenceTypes github-model-data))))


;; {:event "p1469518815647",
;;  :origin "p1469518816773",
;;  :artifact "p1469518815985",
;;  :state-change "p1469518817212",
;;  :collection "p1469518814780",
;;  :log "p1469518813907"}
(github-component-types)


;; {:default 4, :has 1, :implicit 2}
(github-reference-types)


;; (first github/test-issues)

;; github/test-commits


(def i1 {:author-name "JosefWeinbub",
 :time "2016-07-04T18:02:56Z",
 :type :issue,
 :title "German parsetree lemmatising wrong?"})




(comment
  (let [client (ardoq-helper/new-client-from-env)
        workspace-id (:_id (find-github-workspace client))
        model-id (:_id (find-github-model client))]
    {:ws workspace-id
     :ml model-id}))



(defn get-child-set [parent]
  (->> parent
       (map :children)
       flatten
       set))

(defn filter-project [project-name
                      origins
                      collections
                      artifacts
                      state-changes
                      logs
                      events]
  (let [origin (first (filter #(= project-name (:name %)) origins))
        origin-children (set (:children origin))
        project-logs (filter #(origin-children (:_id %)) logs)
        logs-children (get-child-set project-logs)
        project-events (filter #(logs-children (:_id %)) events)
        project-collections (filter #(origin-children (:_id %)) collections)
        collections-children (get-child-set project-collections)
        project-artifacts (filter #(collections-children (:_id %)) artifacts)
        artifacts-children (get-child-set project-artifacts)
        project-state-changes (filter #(artifacts-children (:_id %)) state-changes)]
    {:name project-name
     :origin origin
     :logs project-logs
     :events project-events
     :collections project-collections
     :artifacts project-artifacts
     :state-changes project-state-changes}))

;; TODO
(defn get-ardoq-project-by-origin-name [origin-name]
  (let [c (ardoq-helper/new-client-from-env)
        components (client/find-all (client/map->Component {}) c)
        comp-map (group-by :type components)
        origins (get comp-map "Origin")
        collections (get comp-map "Collection")
        artifacts (get comp-map "Artifact")
        state-changes (get comp-map "StateChange")
        logs (get comp-map "Log")
        events (get comp-map "Event")]
    (filter-project origin-name origins collections artifacts state-changes logs events)))

;; (get-ardoq-project-by-origin-name (str gh-user "/" gh-repo))

(defn create-origin [c]
  (client/create
   (client/->Component
    (str gh-user "/" gh-repo)
    "test"
    ardoq-workspace-id
    ardoq-model-id
    (:origin (github-component-types))
    nil)
   c))

(defn create-issue-collection [c parent-id]
  (client/create
   (client/->Component
    "Issues"
    nil
    ardoq-workspace-id
    ardoq-model-id
    (:collection (github-component-types))
    parent-id)
   c))

(defn max-length-of-30 [title]
  (let [title-length (.length title)]
    (if (< title-length 30)
      title
      (str
       (.substring title 0 26)
       "..."))))

(defn issue-title [issue]
  (str
   "#"
   (:number issue)
   " "
   (max-length-of-30 (:title issue))))

(defn create-issue-component [parent-id issue]
  (let [issue-fields (dissoc issue :type :title :description)]
    (merge
     (client/->Component
      (issue-title issue)
      (StringEscapeUtils/escapeHtml4 (:description issue))
      ardoq-workspace-id
      ardoq-model-id
      (:artifact (github-component-types))
      parent-id)
     issue-fields)))

(defn issues->ardoq [c parent-id]
  (doall
   (for [issue github/test-issues]
     (client/create
      (create-issue-component parent-id issue)
      c))))

(defn create-commit-collection [c parent-id]
  (client/create
   (client/->Component
    "Commits"
    nil
    ardoq-workspace-id
    ardoq-model-id
    (:collection (github-component-types))
    parent-id)
   c))

(defn create-commit-component [parent-id commit]
  (client/->Component
   (max-length-of-30 (:message commit))
   (:sha commit)
   ardoq-workspace-id
   ardoq-model-id
   (:artifact (github-component-types))
   parent-id))

(defn commits->ardoq
  [c parent-id]
  (for [commit github/test-commits]
    (let [created-commit (client/create
                          (merge
                           (create-commit-component parent-id commit)
                           commit)
                          c)]
      created-commit)))

(defn parent-reference [child-id parent-id]
  (when (and (some? parent-id)
             (some? child-id))
    (assoc (client/->Reference ardoq-workspace-id child-id parent-id)
           :type (:parent (github-reference-types)))))

(defn find-parents [commit artifacts]
  (let [parent-shas (set (:parents commit))
        parents (if (empty? parent-shas)
                  #{(:parent commit)}
                  parent-shas)]
  (filterv #(parents (:sha %)) artifacts)))

(defn create-commit-references [c commits]
    (doseq [commit commits
            parent (find-parents commit commits)]
      (let [ref (parent-reference (:_id commit)
                                  (:_id parent))]
        (when (some? ref)
          (client/create
           ref
           c)))))

(defn create-issues [c origin]
  (issues->ardoq c (:_id (create-issue-collection c (:_id origin)))))

(defn create-ardoq-project []
  (let [c (ardoq-helper/new-client-from-env)
        origin (create-origin c)
        issues (create-issues c origin)
        commit-collection (create-commit-collection c (:_id origin))
        commits (commits->ardoq c (:_id commit-collection))
        commit-references (create-commit-references c commits)]))


;;(get-ardoq-project-by-origin-name gh-origin)
;;(create-ardoq-project)
