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
   :cause combine-cause-samples
   :although combine-although-samples
   :single-adjectives combine-single-adjectives-samples
   :adverbs-of-manner combine-adverbs-of-manner-samples
   }
  )


(defn -main []
  (do
    (println "Running samples")
    (println " ")
    (doall
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
             rbh (group-by :hint res)
             sr  #(clojure.string/join (repeat 80 %))
             ]
         (println (sr "="))
         (println "| INPUT   : [" (first smp) "]" )
         (println "|         :   -" k "->" )
         (println "|         : [" (second smp) "]" )
         (println "| CONFIG  :"  (last smp ))
         (println (sr "-"))
         (doall 
          (for [[rk,rv] rbh]
            (do 
              (println "|" rk)
              (doall (map  #(println "|   > " (:sentence  %)) rv)))
            ))
         )
       ))))

(-main)

#_(combiners/combine-cause "We ate ice cream." "The day was hot." {})
#_(combiners/combine-adverbs-of-manner "We climbed the hill." "We climbed rapidly." {})

#_ (tree-seq (comp not nil?) (fn [v] (.getChildrenAsList v)) (:tree  (nlp/sentence-structure "We fed the dog ice cream.")))

#_(combiners/combine-cause "Paul was happy." "Paul was writing clojure." { :NNP [["Paul" "she"]]})
#_ (-> (nlp/sentence-structure "Patrick Henry opposed new British taxes and he gave a speech and the speech was powerful to inspire the colonists.")
       :dep)
