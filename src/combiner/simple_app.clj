(ns combiner.simple-app
  (:require [combiner.combiners :as combiners])
  (:use clojure.string)
  )

(defn make-simple-app []
  (let [dropdowns (partition 2 ["A caused by B"
                                {:fn combiners/combine-cause
                                 :ex ["Jim went to the store." "Jim needed food." "{ :NNP [[\"Jim\" \"he\" ]]}"]
                                 }
                                "A although B"
                                {:fn combiners/combine-although
                                 :ex ["I like ice cream." "I don't like warm milk." "{}"]
                                 }

                                "A modified by single adjective B"
                                {:fn combiners/combine-single-adjectives
                                 :ex [ "The leaves fall from the trees." "The leaves are coloful." {}]
                                 }

                                "A modified by adverb of manner B"
                                {:fn combiners/combine-adverbs-of-manner
                                 :ex [ "The runner ran ten miles." "He ran easily." {}]
                                 }

                                "A modified by adverb from adjective in B"
                                {:fn  combiners/combine-adjectives-to-adverbs
                                 :ex  [ "She plays the piano." "Her playing is beautiful." {}]
                                 }])
        dropdown-map (apply hash-map (flatten dropdowns)) ;; yeah that partition/flatten is dumb. Sorry
        


        frame
        (doto (java.awt.Frame. "Combiner Simple UI")
          (.setSize 900 800)
          (.setLayout (java.awt.BorderLayout.))
          )

        sent-a
        (doto (java.awt.TextField. "This is sentence A."))

        sent-b
        (doto (java.awt.TextField. "This is sentence B."))

        config
        (doto (java.awt.TextField. "{}"))
        
        combiners
        (let [c (java.awt.Choice.)]
          (doall (map #(.add c (first %)) dropdowns))
          c
          )

        combine-button
        (doto (java.awt.Button. "Combine"))

        example-button
        (doto (java.awt.Button. "Populate with Example"))

        combine
        (doto (java.awt.Panel.)
          (.setLayout (java.awt.GridLayout. 1 2))
          (.add combine-button)
          (.add example-button)
          )

        input-panel
        (doto (java.awt.Panel.)
          (.setLayout (java.awt.GridLayout. 8 1))
          (.add (java.awt.Label. "Sentence A"))
          (.add sent-a)
          (.add (java.awt.Label. "Sentence B"))
          (.add sent-b)
          (.add (java.awt.Label. "Configuration"))
          (.add config)
          (.add combiners)
          (.add combine)
          )

        output-area
        (doto (java.awt.TextArea.))
        
        run-combine
        (fn [combiner sa sb config-str]
          (let [op (:fn (get dropdown-map combiner))

                res (op sa sb (binding [*read-eval* false ] (read-string config-str)))
                rbh (group-by :hint res)

                sv  #(clojure.string/join  (repeat 80 %))
                
                as-str
                (with-out-str
                  (doall
                   (for [[rk, rv] rbh]
                     (do 
                       (println (sv "="))
                       (println rk)
                       (doall (map #(println "|  >" (:sentence %)) rv))
                       (println (sv "-"))
                       )
                     
                     ))
                  )
                
                ]
            (.setText output-area as-str)
            )
          )

        populate-example
        (fn []
          (let [comb (.getSelectedItem combiners)
                ex   (:ex (get dropdown-map comb))
                ]
            (.setText sent-a (nth ex 0))
            (.setText sent-b (nth ex 1))
            (.setText config (nth ex 2))
            )
          )
        
        _
        (doto combine-button
          (.addActionListener
           (proxy [java.awt.event.ActionListener] []
             (actionPerformed [e]
               (run-combine (.getSelectedItem combiners) (.getText sent-a) (.getText sent-b) (.getText config))
               )
             )
           )
          )

        _
        (doto example-button
          (.addActionListener
           (proxy [java.awt.event.ActionListener] []
             (actionPerformed [e]
               (populate-example)
               )
             )))
        _
        (doto frame
          (.add input-panel java.awt.BorderLayout/NORTH)
          (.add output-area java.awt.BorderLayout/CENTER)
          (.validate)
          (.setVisible true)
          (.addWindowListener
           (proxy [java.awt.event.WindowAdapter] []
             (windowClosing [e]
               (doto frame
                 (.setVisible false)
                 (.dispose)
                 ))))
          )
        ]
    { :frame  frame }
    )
  )

#_(make-simple-app)

