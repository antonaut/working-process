(ns links-graph.ardoq-helper)

;; Default contents of resources/private/env.json:
;; <pre>
;;    {
;;      "ARDOQ_API_URL"      : "https://app.ardoq.com",
;;      "ARDOQ_API_TOKEN"    : "your-api-token",
;;      "ARDOQ_ORGANIZATION" : "ardoq"
;;    }
;; </pre>

(defn new-client-from-env
  "Creates a new Ardoq client from \"resources/private/env.json\"."
  []
  (let [ardoq-info (json/read-json (slurp (io/resource "private/env.json")))]
    (client/client {:url   (:ARDOQ_API_URL ardoq-info)
                    :token (:ARDOQ_API_TOKEN ardoq-info)
                    :org   (:ARDOQ_ORGANIZATION ardoq-info)})))


(defn find-model-by-name
  "Finds a model in Ardoq with given name."
  [client model-name]
  (->> (client/find-all (client/map->Model {}) client)
       (filter #(= model-name (:name %)))
       first)

(defn find-workspace-by-name
  "Finds a workspace in Ardoq with given name."
  [client workspace-name]
  (->> (client/find-all (client/map->Workspace {}) client)
       (filter #(= workspace-name (:name %)))
       first))

(defn find-all-fields
  [client]
  ;; QUESTION: How to query? Ex. all fields used in a certain workspace
  ;; ANSWER: Filter in client.
  (client/find-all (client/map->Field {}) client))
