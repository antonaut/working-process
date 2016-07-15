(ns links-graph.org-test
  (:require [clojure.test :refer :all]
            [links-graph.org :refer :all]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))


;; A small testcase describing a small .org file
(def t1
  [:org-file
   [:org-header-1 [:title "header111"]
    [:org-header-2 [:title "Header2"]]]
   [:org-header-1 ]
   [:title "t1"]])


;; Node titles
(node-title [:org-header-2
             [:title "header22"]])

(remove #(or (empty? %) (nil? %))
        (map node-title (children t1)))


;; Slightly bigger test case
(def testorg (parse-resource "test.org"))

;;; Traverse the org-tree

;; First level
(->> testorg
     children)

;; 2nd level headers
(->> testorg
     children
     (mapcat children))

;; Transform tree into a seq (depth first traversal)
(defn org-tree-seq
  [org-tree]
  (tree-seq node? children org-tree))

(defn long-str [& strings] (str (clojure.string/join "\n" strings) "\n"))

(def tstr (long-str
           "# Hell.?!oajskdsoj2323a?"
           " asd asd"
           "* One Hello world!"
           "** Two I like this"
           "Some text."
           "*** Three"))

(defn test-view-tree []
  (-> "test2.org"
      parse-resource
      view-tree))

(defn parses-test
  [string]
  (let [org-parser (insta/parser (io/resource "org.bnf"))]
    (insta/parses org-parser string)))

(deftest unambiguous-grammar
  (is
   (= 1 (count (parses-test tstr)))))


(deftest count-headlines

  (testing "The number of headlines in an org string."
    (is (= 3 (->>
              (parse-string (long-str
                             "# Hello"
                             "* One Hello world!"
                             "** Two I like this"
                             "Some text."
                             "*** Three"))
              org-tree-seq
              count
              dec
              ))) ; dec because of 'root' node
    (is (= 8 (->>
              (parse-string (long-str
                             "* One"
                             "** One.One"
                             "** One.Two"
                             "* Two"
                             "** Two.One"
                             "*** Two.One.One"
                             "** Two.Two"
                             "** Two.Three"))
              org-tree-seq
              count
              dec)))))

;;; Graph repesentation
(defn test-graph []
  (let [m1  (org-tree->map-tree t1)
        g1  (map-tree->graph m1)
        id1 (first (first (vals g1)))]
    (find-by-id m1 id1)))

;;; Visualization
;;(view-tree t1)
