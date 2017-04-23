(ns combiner.combiners-test
  (:require [combiner.combiners :as c])
  (use clojure.test)
  )

;; These tests kinda stink. But they at least make sure we run.

(deftest basic-combine-because
  (let  [r (c/combine-cause "We were thirsty." "It was hot." {})
         ]
    (is (= (first  (:good r)) "We were thirsty because it was hot."))
    ))

(deftest basic-combine-although
  (let  [r (c/combine-although "We were thirsty." "We had been drinking water." {})
         ]
    (is (= (first  (:good r)) "We were thirsty, but we had been drinking water."))
    ))

(deftest basic-combine-single-adjectives
  (let [r (c/combine-single-adjectives "I combined sentences." "The sentences were short." {})
        ]
    (is (= (first (:good r)) "I combined short sentences."))
    )
  )

(deftest basic-combine-adverbs-of-manner
  (let [r (c/combine-adverbs-of-manner "I combined sentences." "I combined them correctly." {})
        ]
    (is (= (first (:good r)) "I correctly combined sentences."))
    )
  )
