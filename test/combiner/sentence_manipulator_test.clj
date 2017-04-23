(ns combiner.sentence-manipulator-test
  (:require [combiner.sentence-manipulator :as sm])
  (:require [combiner.nlp-mechanics :as nlp])
  (:use clojure.test)
  )

(deftest sm-basics
  (let [ss1 (nlp/sentence-structure "Food is nice.")
        s1  (:sent ss1)
        p   (concat [(sm/insert-el "the")] (drop-last  s1)) 
        q   (sm/case-fix p)
        r   (sm/sentence-string q)

        ;; Note the absence of a drop last
        p2  (->  (concat [(sm/insert-el "the")] s1) 
                 (sm/case-fix)
                 (sm/sentence-string)
                 ) 
        ]
    (is (= r "The food is nice."))
    (is (= p2 "The food is nice."))
    )
  )

(deftest sm-invertible
  (doall
   (for [tcase ["The food is nice."
                "We gave the book to Jim."
                ] ]
     (do
       (let [p  (-> (nlp/sentence-structure tcase)
                    :sent
                    drop-last
                    (sm/case-fix)
                    (sm/sentence-string)
                    )]
         (is (= p tcase))
         ))
     ))
  (doall
   (for [tcase ["We love fun!"
                "Jim didn't understand the problem."
                "Jim was running quickly, then he fell down the deep well without recourse!"
                "Walruses are animals."
                "Despite the long time it took, we ended up finding Frank underneath the desk."
                ] ]
     (do
       (let [p  (-> (nlp/sentence-structure tcase)
                    :sent
                    (sm/case-fix)
                    (sm/sentence-string)
                    )]
         (is (= p tcase))
         ))
     ))
  )

(deftest nnpfix-test
  (let [s (nlp/sentence-structure "Jim went to the beach and Jim had a beer.")
        r (sm/multi-nnp-fix { :NNP [["Jim" "he"]]} (:sent s))
        fin ( -> r (sm/case-fix) (sm/sentence-string))
        ]
    (is (= fin "Jim went to the beach and he had a beer."))
    )

  #_(let [s (nlp/sentence-structure "Frank was on the farm, and a dog attacked Frank.")
          r (sm/multi-nnp-fix { :NNP [["Frank" "he"]]} (:sent s))
          fin ( -> r (sm/case-fix) (sm/sentence-string))
          ]
      (is (= (:sent s) "Jim went to the beach and he had a beer."))
      )

  )

