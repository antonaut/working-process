(ns links-graph.core-test
  (:require [clojure.test :refer :all]
            [links-graph.core :refer :all]
            ))

(def ardoq-fields
  [{:label               "Criticality"                                 ,
    :name                "Criticality"                                 ,
    :type                "list"                                        ,
    :model               "56dd3f389f2a260e0d947a5a"                    ,
    :componentType       nil,
    :description         "This criticality... "                        ,
    :last-updated        "2016-03-07T08:43:36.417Z"                    ,
    :_id                 "56dd3f389f2a260e0d947a5d"                    ,
    :lastModifiedByName  "Anton Erholt"                                ,
    :global              true,
    :createdByEmail      "anton.erholt@gmail.com"                      ,
    :created             "2016-03-07T08:43:36.417Z"                    ,
    :created-by          "56dd3f379f2a260e0d947a53"                    ,
    :last-modified-by    "56dd3f379f2a260e0d947a53"                    ,
    :origin              {:_version 2, :id "54ddcd23e4b0be60044414e9"} ,
    :lastModifiedByEmail "anton.erholt@gmail.com"                      ,
    :defaultValue        "high, medium, low"                           ,
    :_version            1,
    :createdByName       "Anton Erholt"}
   {:label               nil,
    :name                "CIDER"                                       ,
    :type                "Text"                                        ,
    :model               "56dd3f389f2a260e0d947a57"                    ,
    :componentType       ["p1375641998859"]                            ,
    :description         ""                                            ,
    :last-updated        "2016-03-07T08:43:36.417Z"                    ,
    :_id                 "56dd3f389f2a260e0d947a5f"                    ,
    :lastModifiedByName  "Anton Erholt"                                ,
    :createdByEmail      "anton.erholt@gmail.com"                      ,
    :created             "2016-03-07T08:43:36.417Z"                    ,
    :created-by          "56dd3f379f2a260e0d947a53"                    ,
    :last-modified-by    "56dd3f379f2a260e0d947a53"                    ,
    :origin              {:_version 1, :id "542a7788e4b0e66cd9b613a2"} ,
    :lastModifiedByEmail "anton.erholt@gmail.com"                      ,
    :_version            1,
    :createdByName       "Anton Erholt"}])


;; A small testcase describing a small .org file
(def t1
  [:org-file
   [:org-header-1 [:title "header111"]
    [:org-header-2 [:title "Header2"]]]
   [:org-header-1 ]
   [:title "t1"]])


(deftest get-org-fields-from-component-type
  (is
   (= true (some #(field-on-component-types? % {:org-header "p1375641998859" }) ardoq-fields))))


(def three-headers-graph
  {:one-hello-world #{:two-i-like-this}
   :two-i-like-this #{:three}
   }) ;    :three []

(def few-headers-graph
  {:one     #{:one-one
              :one-two}
   :two     #{:two-one
              :two-two
              :two-three}
   :two-one #{:two-one-one}})
