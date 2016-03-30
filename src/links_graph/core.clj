;; This is a small application for showing the headlines of an .org
;; file like a graph (mind map).

(ns links-graph.core)

;; Visualises a .org file
(defn visualize-file
  [filename]
  (org/parse-file filename))

;; Parses a .org string
(defn parse-org-string
  [string]
  (org/parse-file (.toCharArray string)))

;; IDEA - I'd like to make a small test case. Like really small.
;; JUST MAKE A TEST

;; TODO Read a lil bit more on how organum data is ordered.
;; TODO Make the graph transform with specter.

(def g1 {:a {:b {:weight 1}
             :c {:weight 2}}
         :b {:a {:weight 2}
             :c {:weight 4}}
         :c {}})


(def bfs
  [start end successors]
  (let [queue clojure.lang.PersistentQueue/EMPTY]
    ))


(viz/view-graph
 (keys g1) g1
 :node->descriptor (fn [n] ({:label n})))

(def aa [{:type :root, :content [{:line-type :blank, :text ""}]}
         {:type :section,
          :content [],
          :level 1,
          :name "One Hello world!",
          :tags nil,
          :kw nil}
         {:type :section,
          :content [{:line-type :table-row, :text "   Some text."}],
          :level 2,
          :name "Two I like this",
          :tags nil,
          :kw nil}
         {:type :section, :content [], :level 3, :name "Three", :tags nil, :kw nil}])

;;;;  TODO Go from a list of things to a graph.

;; Parse a list of org-nodes as a vertices and returns an org-graph
(defn org-nodes-into-graph
  [nodes]
  (loop [node        (first nodes)
         nodes       nodes
         last-level  0
         last-vertex nil
         graph       {}]
    (if (empty? nodes)
      graph
      (when-let [current-level      (:level node)
                 current-vertex     (keyword (:name node))
                 current-level-diff (- last-level current-level)]
        (pos? current-level-diff) (recur
                                   (first nodes)
                                   nodes
                                   current-level
                                   current-vertex
                                   ( )) ; descend the tree
        (zero? current-level-diff)
        (recur current nodes )))))

(defn graphify
  [tree]
  (->> tree
       (filter #(and (:level %) (:name %)))))

(defn make-graph
  [tree]
  (into {} graphify tree))

(defn -main []
  (visualize-file "../literature-review/literature-review.org"))
