(ns combiner.wordnet-mechanics-test
  (:require [combiner.wordnet-mechanics :as wnm])
  (:use clojure.test)
  )

(deftest nearest-adverb-test
  (is (= (wnm/nearest-adverb-from-adjective "loud") "loudly"))
  (is (= (wnm/nearest-adverb-from-adjective "gentle") "gently"))
  )
