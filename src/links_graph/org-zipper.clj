;;; # org-tree-zipper is not used
;;; It is just a cave where ideas can grow.

(ns links-graph.org-tree-zipper
  (:require [clojure.zip :as z]
            [links-graph.org :refer :all]))

;;; IDEA: Use a zipper to traverse the tree and replace org-nodes with ardoq-entities.

(defn org-tree->zipper
  [org-tree]
  (z/zipper
   node?
   children
   (fn [n children]
     (vec (cons n children)))
   org-tree))

;; Thanks for tree-edit, Alex Miller (alex@puredanger.com)
;; http://www.ibm.com/developerworks/library/j-treevisit/

(defn tree-edit
  "Edits every zipper node with editor fn."
  [zipper editor]
  (loop [loc zipper]
    (if (z/end? loc)
      (z/root loc)
      (recur (z/next (z/edit loc editor))))))

(comment
  (defn zipper->seq
    [zipper]
    (loop [loc   zipper
           nodes ()]
      (if (z/end? loc)
        nodes
        (let [node (z/node loc)]
          (if-not (nil? node)
            (recur (z/next loc) (cons node nodes))
            (recur (z/next loc) nodes)))))))


(comment
  ;; Constuction and usage of a zipper
  (let [org-tree    (parse-file "./resources/test.org")
        tree-zipper (z/zipper node? children identity org-tree)]
    (->
     tree-zipper
     z/down
     z/down
     z/node
     (node-attr :title))))
