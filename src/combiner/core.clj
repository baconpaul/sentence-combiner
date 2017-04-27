(ns combiner.core
  (:require [combiner.combiners :as combiners])
  (:require [combiner.nlp-mechanics :as nlp])
  (use clojure.java.io)
  (use clojure.java.shell)
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

;; NOTE: Shares a lot with adverbs of manner once you get to the adverb.
(def combine-adjectives-to-adverbs-samples
  [
   [ "She plays the piano." "Her playing is beautiful." {}]
   [ "He smiles." "He is happy." {}]
   [ "Lin waited for the movie to begin." "Lin was patient." {}]
   [ "Sasha cheered." "Sasha was excited." {}]
   ]
  )

(def combine-and-samples
  [
   [ "Some penguins eat fish." "Some eat squid." {} ]
   ])

(def combine-samples
  {
   :cause combine-cause-samples
   :although combine-although-samples
   :single-adjectives combine-single-adjectives-samples
   :adverbs-of-manner combine-adverbs-of-manner-samples
   :adjectives-to-adverbs combine-adjectives-to-adverbs-samples
   }
  )

;; Lovely function for an intern to fix up.
(defn samples-to-html []
  (with-out-str
    (println (clojure.string/join
              "\n"
              [
               "<html><head><style type=\"text/css\">"
               "div { font-size: 12pt; font-family: sans-serif; }"
               ".input { margin-top: 20px; background: #eeeeff; font-weight: bold; }"
               ".op { font-weight: normal; }"
               ".cfg { font-weight: normal; }"
               ".hint { background: #ffeeee; font-style: italic; }"
               ".result { margin-left: 20px; }"
               ".error {margin-left: 20px; font-family: courier; }"
               "</style></head><body>"
               ]))
    (do
      (doall
       (for [[k,v] combine-samples
             smp   v
             ]
         (let [cfn (condp = k
                     :cause combiners/combine-cause
                     :although combiners/combine-although
                     :single-adjectives combiners/combine-single-adjectives
                     :adverbs-of-manner combiners/combine-adverbs-of-manner
                     :adjectives-to-adverbs combiners/combine-adjectives-to-adverbs
                     )
               res (apply cfn smp)
               rbh (group-by :hint res)
               sr  #(clojure.string/join (repeat 80 %))
               ]
           (println "<div class=\"input\">" (first smp) "<span class=op>" k "</span>" (second smp) "<span class=cfg>" (last smp) "</span></div>")
           (doall 
            (for [[rk,rv] rbh]
              (do 
                (println "<div class=hint>" rk "</div>")
                (if (= rk :system-error)
                  (doall (map  #(println "<div class=error>" (:sentence  %) "</div>") rv))
                  (doall (map  #(println "<div class=result>" (:sentence  %) "</div>") rv))))
              ))
           )
         )))
    (println "</body></html>")
    )
  )

(defn show-html []
  (let [s (samples-to-html)
        f "/tmp/comb.html"
        ]
    (with-open [wrtr (writer f)]
      (.write wrtr s))
    (sh "open" f)
    )
  )

(defn -main []
  (do
    (show-html)
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
                   :adjectives-to-adverbs combiners/combine-adjectives-to-adverbs
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



