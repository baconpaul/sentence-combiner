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
  )

(deftest find-in-tree-test
  (let [s  (nlp/sentence-structure "Robert climbed the mountain.")
        vp (sm/find-in-tree s #(= (.toString (.label %)) "VP"))
        v2 (sm/find-label-in-tree s "VP")
        ]
    (is (= (.toString  (first  vp)) "(VP (VBD climbed) (NP (DT the) (NN mountain)))"))
    (is (= (count vp) 1))
    (is (= (map #(.toString (.label %)) (.getChildrenAsList (first vp))) ["VBD" "NP"]))
    (is (= vp v2))
    )
  )

(deftest omit-punct-test
  (let [s (:sent (nlp/sentence-structure "Paul was happy, but Fred was short."))
        r (sm/omit-each-punct s)
        t (map sm/sentence-string r)
        ]
    (is (= (count r) 2))
    (is (= (first t) "Paul was happy but Fred was short."))
    (is (= (second t) "Paul was happy, but Fred was short"))
    ))


