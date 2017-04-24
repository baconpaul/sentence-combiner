(ns combiner.combiners
  (:require [combiner.nlp-mechanics :as nlp])
  (:require [combiner.sentence-manipulator :as sm])
  )

;; OK so lets start with some cause and effect
(defn combine-cause
  "Given two sentences, where A is caused by cause, combine using because and so and so on"
  [a cause config]
  (let [ss-a         (nlp/sentence-structure a) ;; For now assume they are punctuated
        ss-cause     (nlp/sentence-structure cause)

        pos-a        (drop-last (:sent ss-a))
        pos-cause    (drop-last (:sent ss-cause))
        
        good      (->>  [
                         (concat pos-a [(sm/insert-el "because")] pos-cause)
                         (concat pos-cause [sm/comma (sm/insert-el "so")] pos-a)
                         (concat [(sm/insert-el "because")] pos-cause [sm/comma] pos-a)
                         ]
                        (map sm/case-fix)
                        (map (partial  sm/multi-nnp-fix config))
                        (map sm/sentence-string)
                        (map (fn [ v] { :sentence v :hint :correct }))
                        )
        soso      (->> [
                        {:sentence (concat pos-cause [(sm/insert-el "and") ] pos-a) :hint :better-combiner}
                        ]
                       (map (fn [v] (update v :sentence sm/case-fix)) )
                       (map (fn [v] (update v :sentence sm/sentence-string)) )

                       )
        wrong     (->> [
                        {:sentence (concat pos-cause [(sm/insert-el "because") ] pos-a) :hint :wrong-order}
                        {:sentence (concat pos-cause [(sm/insert-el "although") ] pos-a) :hint :wrong-order}
                        ]
                       (map (fn [v] (update v :sentence sm/case-fix)) )
                       (map (fn [v] (update v :sentence sm/sentence-string)) )

                       )
        
        ]
    (concat good soso wrong )
    )
  
  )

(defn combine-although
  "Given two sentences A and Oppose-A combine them with but and although and stuff"
  [a oppose-a config]
  (let [ss-a         (nlp/sentence-structure a) ;; For now assume they are punctuated
        ss-oppose    (nlp/sentence-structure oppose-a)

        pos-a        (drop-last (:sent ss-a))
        pos-oppose   (drop-last (:sent ss-oppose))
        
        good      (->>  [
                         (concat pos-a [sm/comma (sm/insert-el "but")] pos-oppose)
                         (concat [(sm/insert-el "although")] pos-oppose [ sm/comma ] pos-a)
                         ]
                        (map sm/case-fix)
                        (map (partial  sm/multi-nnp-fix config))
                        (map sm/sentence-string)
                        (map (fn [ v] {:sentence v :hint :correct}))
                        )
        soso      (->> [
                        ]
                       )
        wrong     (->> [
                        ]
                       )
        
        ]
    (concat good soso wrong)
    ))


(defn combine-single-adjectives
  "I combined sencentes. The sentences were short. -> I combined short sentences"
  [stmt adjclause config]
  (let [ss-stmt         (nlp/sentence-structure stmt) ;; For now assume they are punctuated
        ss-adjclause    (nlp/sentence-structure adjclause)

        ;; OK so the adjectival sentences generally parse out with a subject which is a noun and a
        ;; root which is an adjective. So lets look for those
        cand-subj       (filter #(and
                                  (:nsubj (:grammar %))
                                  (or  (= "NN" (:pos %))
                                       (= "NNS" (:pos %))
                                       )
                                  ) (:sent  ss-adjclause))
        cand-adj        (filter #(and
                                  (:root (:grammar %))
                                  (= "JJ" (:pos %))
                                  ) (:sent  ss-adjclause))

        _               (if (not (and (= 1 (count cand-adj))
                                      (= 1 (count cand-subj))
                                      ))
                          (throw (ex-info "Didn't find exactly 1 subject and adjective in clause 2"
                                          { :cand-subj cand-subj
                                           :cand-adj cand-adj
                                           :clause adjclause
                                           :ss-adjclause ss-adjclause}
                                          ))
                          )
        subj            (first cand-subj)
        adj             (first cand-adj)

        ;; So OK now we need to start creating some combinations. Lets do the simplest of just
        ;; insert the adjective before the subject.
        split-subj        (split-with
                           #(not (= (clojure.string/lower-case (:word subj))
                                    (clojure.string/lower-case  (:word %))))
                           (drop-last (:sent  ss-stmt)))
        
        ;; ERROR CHECKING HERE PLEASE
        res   (->  (concat (first split-subj) [adj] (second split-subj))
                   (sm/case-fix)
                   (sm/sentence-string)
                   )
        ]
    
    [{:sentence res :hint :correct} ]
    
    
    )
  )

(defn combine-adverbs-of-manner
  "Paul combined the pair of sentences. Paul combined correctly. -> Paul correctly combined the pair of sentences."
  [stmt adv-clause config]
  (let [ss-stmt         (nlp/sentence-structure stmt) ;; For now assume they are punctuated
        ss-advclause    (nlp/sentence-structure adv-clause)

        ;; OK so the advectival sentences generally parse out with a subject which is a noun and a
        ;; root which is an advective. So lets look for those
        cand-vtgt       (filter #(and
                                  (:root (:grammar %))
                                  (or  (= "VBD" (:pos %))
                                       )
                                  ) (:sent  ss-advclause))
        cand-adv        (filter #(and
                                  (:advmod (:grammar %))
                                  (= "RB" (:pos %))
                                  ) (:sent  ss-advclause))

        _               (if (not (and (= 1 (count cand-adv))
                                      (= 1 (count cand-vtgt))
                                      ))
                          (throw (ex-info "Didn't find exactly 1 verb target and adverbal modifier in clause 2"
                                          { :cand-vtgt cand-vtgt
                                           :cand-adv cand-adv
                                           :clause adv-clause
                                           :ss-advclause ss-advclause}
                                          ))
                          )
        vtgt            (first cand-vtgt)
        adv             (first cand-adv)
        ]
    (->> (concat
          ;; Adjective before verb - correct
          (let [split-subj        (split-with
                                   #(not (= (clojure.string/lower-case (:word vtgt)) ;; Conjugation will make this fail
                                            (clojure.string/lower-case  (:word %))))
                                   (drop-last (:sent  ss-stmt)))
                
                ;; ERROR CHECKING HERE PLEASE
                resa   (->  (concat (first split-subj) [adv] (second split-subj))
                            (sm/case-fix)
                            )
                res    (sm/sentence-string resa)
                rnp    (sm/sentence-string (filter #(not (= (:pos %) ".")) resa))
                ]
            
            [
             {:sentence res :hint :correct }
             {:sentence rnp :hint :punctuation }
             ]
            )
          ;; Adverb after noun phrase - correct
          (try  (let [ ;; Now if we find the span of the verb phrase we can also insert it correct after that
                      vp    (sm/find-label-in-tree ss-stmt "VP")
                      np    (if (= (count vp) 1) (first vp) (throw (ex-info "Got more than one VP in statement" { :vp vp :ss-stmt ss-stmt })))
                      npch  (filter #(= (.toString (.label %)) "NP")  (.getChildrenAsList np))
                      fnp   (if (= (count npch) 1) (first npch) (throw (ex-info "Got more than one NP in VP statement" {:np np :ss-stmt ss-stmt})))
                      wrdnp (sm/tree-terminal-strings fnp)

                      ;; For now make a bad assumption that the last word of the nounphrase is the one we want
                      sw    (last wrdnp)
                      split-subj        (split-with
                                         #(not (= (clojure.string/lower-case sw)
                                                  (clojure.string/lower-case (:word %))))
                                         (drop-last (:sent  ss-stmt)))
                      resa   (->  (concat (first split-subj)  [(first (second split-subj)) adv] (rest  (second split-subj)))
                                  (sm/case-fix)
                                  )
                      res    (sm/sentence-string resa)
                      rnp    (sm/sentence-string (filter #(not (= (:pos %) ".")) resa))
                      ]
                  [
                   {:sentence res :hint :correct }
                   {:sentence rnp :hint :punctuation }
                   ]
                  )
                (catch Exception  e (do (println e) [nil])))
          )
         (filter (comp not nil?))
         )
    )
  )



