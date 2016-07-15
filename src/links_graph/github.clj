(ns links-graph.github
  (:require [tentacles.users]
            [tentacles.repos]
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

(defn my-emails
  []
  (tentacles.users/emails default-options))

(defn my-repos
  []
  (tentacles.repos/user-repos "antonaut" default-options))
