(ns combiner.nlp-mechanics-test
  (:require [combiner.nlp-mechanics :as nlp])
  (:use clojure.test)
  )

(deftest nlp-mech-basics
  (let [c1  (nlp/sentence-structure "The dog is nice.")
        ]
    (is (not (nil? (:sent c1))))
    (is (not (nil? (:tree c1))))
    (is (not (nil? (:dep c1))))

    (let [s  (:sent c1)
          t  (:tree c1)
          d  (:dep c1)
          ]
      (is (= (count s) 5))
      (is (= (map :word s) ["The" "dog" "is" "nice" "."]))
      (is (= (map :pos s) ["DT" "NN" "VBZ" "JJ" "."]))
      (is (= (map :grammar s) [#{:det} #{:nsubj} #{:cop} #{:root} #{:punct}]))
      )
    ))
