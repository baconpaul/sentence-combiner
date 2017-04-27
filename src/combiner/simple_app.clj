(ns combiner.simple-app
  (:require [combiner.combiners :as combiners])
  (:use clojure.string)
  )

(defn make-simple-app []
  (let [frame
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
        (doto (java.awt.Choice.)
          (.add "A caused by B")
          (.add "A although B")
          (.add "A modified by single adjective B")
          (.add "A modified by adverb of manner B")
          (.add "A modified by adverb from adjective in B")
          )
        
        combine
        (doto (java.awt.Button. "Combine")
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
          (let [op (condp = combiner ;; Remember need to add these above also (sucky)
                     "A caused by B" combiners/combine-cause
                     "A although B" combiners/combine-although
                     "A modified by single adjective B" combiners/combine-single-adjectives
                     "A modified by adverb of manner B" combiners/combine-adverbs-of-manner
                     "A modified by adverb from adjective in B" combiners/combine-adjectives-to-adverbs
                     :else str
                     )

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
        
        _
        (doto combine
          (.addActionListener
           (proxy [java.awt.event.ActionListener] []
             (actionPerformed [e]
               (run-combine (.getSelectedItem combiners) (.getText sent-a) (.getText sent-b) (.getText config))
               )
             )
           )
          )
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

#_
(make-simple-app)
