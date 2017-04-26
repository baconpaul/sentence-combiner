(ns combiner.wordnet-mechanics
  (:import (net.sf.extjwnl.dictionary Dictionary)
           (net.sf.extjwnl.data POS PointerUtils)
           )
  (:require clojure.string)
  )



(defn nearest-adverb-from-adjective
  "Slow -> slowly. Quick -> quickly. Quick -> rapidly is rejected (ahtough we know it)"
  [seeking]
  (let [dict     (Dictionary/getDefaultResourceInstance)
        all-adv  (iterator-seq  (.getSynsetIterator dict POS/ADVERB))

        targets  (->> (map #(iterator-seq (.iterator  (.getTargets %))) all-adv)
                      (filter (comp not nil?))
                      (flatten)
                      (filter #(and (not (nil? (.getPOS %))) (= (.getPOS %) POS/ADJECTIVE)))
                      (filter #(= (.getLemma %) seeking))
                      (count)
                      )

        tAsFilter (fn [el]
                    (and
                     (not (nil? el))
                     (> 
                      (->> (iterator-seq (.iterator (.getTargets  el)))
                           (filter #(and (not (nil? (.getPOS %))) (= (.getPOS %) POS/ADJECTIVE)))
                           (filter #(= (.getLemma %) seeking))
                           (count)

                           )
                      0
                      )
                     )
                    )
        adverb-candidates   (filter tAsFilter all-adv)
        adverb-words        (->>  (map #(iterator-seq (.iterator (.getWords %)))
                                       adverb-candidates
                                       )
                                  flatten
                                  (filter #(= (.getPOS %) POS/ADVERB))
                                  set
                                  vec
                                  )

        adverb-lemmas (map #(.getLemma %) adverb-words)
        
        ;; And now a heuristic. How many starting characters do we have in common? And pick the one ending with ly if there is one.
        sim-score     (map
                       (fn [w]
                         (+
                          (count (take-while #(= (first %) (second %)) (partition 2 (interleave w seeking))))
                          (if  (clojure.string/ends-with? w "ly") 3 0)
                          ))
                       adverb-lemmas
                       )

        scored-lemmas  (partition 2 (interleave adverb-lemmas sim-score))

        result   (first  (apply max-key second scored-lemmas))
        
        ]
    result
    ))


