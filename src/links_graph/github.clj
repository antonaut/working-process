(ns links-graph.github
  (:require [tentacles.users]
            [tentacles.repos]
            [tentacles.events]
            [tentacles.data]
            [tentacles.issues]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
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

;;(def gh-user "antonaut")
;;(def gh-repo "working-process")

;; Test repository: https://github.com/clips/pattern
(def gh-user "clips")
(def gh-repo "pattern")

(defn my-emails
  []
  (tentacles.users/emails default-options))

(defn my-repos
  []
  (tentacles.repos/user-repos gh-user default-options))


;;(tentacles.events/repo-events "antonaut" "working-process")

;;(def master-commit-obj
;;  (-> (tentacles.data/references gh-user gh-repo default-options)
;;      first
;;      :object))

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

;;(parent-commits (get-commit (:sha master-commit-obj)))

(defn issue-label-frequencies [repo]
  (tentacles.issues/repo-labels gh-user gh-repo default-options))

;;(tentacles.issues/issues gh-user gh-repo default-options)
;;(tentacles.repos/commits gh-user gh-repo default-options)

(def github-test-data
  (edn/read
   (java.io.PushbackReader.
    (io/reader
     (io/resource "github-test.edn")))))


;; ISSUES

(def issues-test-data (:issues github-test-data))

;; (first commits-test-data)

(defn issue->vis-issue [m]
  (hash-map
     :time (:updated_at m)
     :title (:title m)
     :author-name (:login (:user m))))


(def test-issues
  (map
   issue->vis-issue
   issues-test-data))

;; COMMITS

(def commits-test-data (:commits github-test-data))

(defn commit->vis-commit [m]
  (hash-map
   :author-name (get-in m [:commit :author :name])
   :message (get-in m [:commit :message])
   :sha (:sha m)
   :time (get-in m [:commit :author :date])
   :parents (mapv :sha (:parents m))))

(def test-commits
  (mapv
   commit->vis-commit
   commits-test-data))

;; (->  (nth test-commits 27)
;;      :message)

;; (map gh-repo->ardoq my-repos)
