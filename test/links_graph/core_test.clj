(ns links-graph.core-test
  (:require [clojure.test :refer :all]
            [links-graph.core :refer :all]))

(def three-headers
  "
* One Hello world!
** Two I like this
   Some text.
*** Three")

(def three-headers-graph
  {:one-hello-world #{:two-i-like-this}
   :two-i-like-this #{:three}
   }) ;    :three []


(def few-headers
  "
* One
** One.One
** One.Two
* Two
** Two.One
*** Two.One.One
** Two.Two
** Two.Three
")

(def few-headers-graph
  {:one #{:one-one
          :one-two}
   :two #{:two-one
          :two-two
          :two-three}
   :two-one #{:two-one-one}})

                                        ;:one-one []
                                        ;:one-two []
                                        ;:two-two []
                                        ;:two-three []
                                        ;:two-one-one []


(deftest count-headlines
  (testing "The number of headlines in an org file."
    (is (= 3 (->>
              (parse-org-string three-headers)
              count
              dec))) ; dec because of 'root' node
    (is (= 8 (->>
              (parse-org-string few-headers)
              count
              dec)))))
