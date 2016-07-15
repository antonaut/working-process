(ns links-graph.github
  (:require [tentacles.users]
            [tentacles.repos]
            [tentacles.events]
            [tentacles.data]
            [clojure.data.json :as json]))

(def auth-str
  (clojure.string/join
   ":"
   (vals
    (json/read-json
     (slurp (clojure.java.io/resource "private/github.json"))))))

(def user-agent
  "master thesis project - @antonaut")

(def default-options
  {:user-agent user-agent
   :auth       auth-str})

(def gh-user "antonaut")
(def gh-repo "working-process")


(defn my-emails
  []
  (tentacles.users/emails default-options))

(defn my-repos
  []
  (tentacles.repos/user-repos gh-user default-options))



;;(tentacles.events/repo-events "antonaut" "working-process")

(def master-commit-obj (-> (tentacles.data/references gh-user gh-repo default-options)
                       first
                       :object))


(defn get-commit [sha]
  (tentacles.data/commit gh-user gh-repo sha default-options))

(defn parent-commits [commit]
  (loop [commit commit
         cnt 10]
    (when (and (not= 404 (:status commit))
               (> 0 cnt))
      (do
        (println commit)
        (recur (get-commit (:sha (first (:parents commit)))) (dec cnt))))))

(parent-commits (get-commit (:sha master-commit-obj)))

(tentacles.data/tree gh-user gh-repo "3ead18a654bfdb80187393e266c2193c7929c7e6" default-options)


;; (map gh-repo->ardoq my-repos)
