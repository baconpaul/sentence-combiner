(ns combiner.core
  (:require [combiner.combiners :as combiners])
  (:require [combiner.nlp-mechanics :as nlp])
  )


;; Core is just sample cases for now. Eventually we will eliminate this
(def combine-cause-samples
  [["We were sad." "Jim's dog died last week." {}]
   ["Hillary didn't become president." "Hillary lost Michigan." {:NNP [["Hillary" "she"]]}]
   ["Frank walked to school." "Frank lost his bike." {:NNP [["Frank" "he"]] }]
   ["Frank was scared of dogs." "A dog attacked Frank." {:NNP [["Frank" "he"]]}]
   ]
  )

(def combine-although-samples
  [["George Washington freed the colonies." "George Washington didn't free his slaves." {:NNP [["George Washington" "he"]]}]
   ["I like ice cream." "I don't like warm milk." {}]
   ["The organization is based in Europe." "The organization has 132 members in America." {}]
   ]
  )

(def combine-single-adjectives-samples
  [
   ["The gems glittered in the sunlight." "The sunlight was bright." {}]
   ["The leaves fall from the trees." "The leaves are colorful." {}]
   ["Honey oozed from the jar." "The honey was sticky." {}]
   ["Bacon sizzled in the pan." "The pan was hot." {}]
   ["I bit into the peach." "The peach was tasty." {}]
   ])

(def combine-adverbs-of-manner-samples
  [
   ["LeBron James dunked the basketball." "He dunked it powerfully." {}]
   ["The runner ran ten miles." "He ran easily." {}]
   ["Maria gave a speech to the school." "She gave the speech confidently." {}]
   ;; ["Jose welcomed the new student." "He welcomed her warmly." {}] 
   ["Robert climbed the mountain." "He climbed bravely." {}]
   ["Miles painted the model airplane." "He painted it carefully." {}]
   ])

(def combine-samples
  {
   ;;:cause combine-cause-samples
   ;; :although combine-although-samples
   ;;:single-adjectives combine-single-adjectives-samples
   :adverbs-of-manner combine-adverbs-of-manner-samples
   }
  )


(do
  (println " ")
  (println " ")
  (for [[k,v] combine-samples
        smp   v
        ]
    (let [cfn (condp = k
                :cause combiners/combine-cause
                :although combiners/combine-although
                :single-adjectives combiners/combine-single-adjectives
                :adverbs-of-manner combiners/combine-adverbs-of-manner
                )
          res (apply cfn smp)
          sr  #(clojure.string/join (repeat 80 %))
          ]
      (println (sr "="))
      (println "| INPUT   : [" (first smp) "]" )
      (println "|         :   -" k "->" )
      (println "|         : [" (second smp) "]" )
      (println "| CONFIG  :"  (last smp ))
      (println (sr "-"))
      (println "| GOOD : ")
      (doall (map  #(println "|   > " %) (:good res)))
      (println "| SOSO : ")
      (doall (map  #(println "|   > " %) (:soso res)))
      (println "| WRONG: ")
      (doall (map  #(println "|   > " %) (:wrong res)))
      (println (sr "-"))
      )
    ))

(-> combine-adverbs-of-manner-samples
    (nth 3)
    (nth 1)
    (nlp/sentence-structure )
    )


