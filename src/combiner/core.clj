(ns combiner.core
  (:require [combiner.combiners :as combiners])
  )


;; OK so here is the combining logic once I've done that mechanics


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

(def combine-samples
  {:cause combine-cause-samples
   :although combine-although-samples
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

