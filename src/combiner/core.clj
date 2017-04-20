(ns combiner.core
  (:import (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
           (edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation
                                  CoreAnnotations$TokensAnnotation
                                  CoreAnnotations$TextAnnotation
                                  CoreAnnotations$PartOfSpeechAnnotation
                                  CoreAnnotations$NamedEntityTagAnnotation
                                  )
           (edu.stanford.nlp.trees TreeCoreAnnotations$TreeAnnotation)
           (edu.stanford.nlp.semgraph SemanticGraphCoreAnnotations$BasicDependenciesAnnotation)
           )
  )

(defn new-pipeline []
  (let [p       (doto  (java.util.Properties.)
                  (.setProperty "annotators" "tokenize, ssplit, pos, lemma, ner, parse, depparse, dcoref")
                  (.setProperty "ssplit.isOneSentence" "true")
                  )
        pl      (StanfordCoreNLP. p )
        ]
    pl
    ))

(defn annotate [txt]
  (let [doc (Annotation. txt)
        p   (new-pipeline)]
    (.annotate p doc)
    doc
    ))

(defn sentence-structure [sent]
  (let [doc    (annotate sent)
        sents  (.get doc CoreAnnotations$SentencesAnnotation)
        ;; Put a len sent check here to make sure it is one
        sent   (first sents)
        toks   (.get sent CoreAnnotations$TokensAnnotation) 
        tok_to_map (fn [t] {:word (.get t CoreAnnotations$TextAnnotation)
                            :pos (.get t CoreAnnotations$PartOfSpeechAnnotation)
                            :ner (.get t CoreAnnotations$NamedEntityTagAnnotation)
                            })

        dep  (.get sent SemanticGraphCoreAnnotations$BasicDependenciesAnnotation)

        sentmap     (map tok_to_map toks)
        
        depl        (map #(.getNodeByIndex dep (inc  %)) (range (count sentmap)))
        depedge     (map #(.incomingEdgeList dep %) depl )
        depann      (map #(if (not (empty? %)) (map (fn [s] (keyword (.toString  (.getRelation s)))) %) [:root] ) depedge)

        sentgram    (->> (map vector
                              (map #(.lemma %) depl)
                              (map #(apply hash-set %) depann)
                              (map #(.index %) depl)
                              )
                         (map  #(apply hash-map (interleave [ :lemma :grammar :idx] %)))
                         )

        sentmap-w-gram (map (fn [w g] (assoc w :grammar (:grammar g))) sentmap sentgram )

        res {:sent  sentmap-w-gram
             :tree  (.get sent TreeCoreAnnotations$TreeAnnotation)
             :dep   dep
             }
        
        ]
    res
    ))


;; OK so here is the combining logic once I've done that mechanics

(defn insel [s] { :word s :pos "INSERTED" :ner "O"})
(defn inspair [s] { :word s :pos s :ner "O"})


(defn case-fix
  "Given a collection of POS tagged words perhaps with manual inserts, fix the capitalization.
  We assume that we are a single sentence, and for now assume POS as 'NNP' is the indicator of proper noun (but should use ner later)"
  [s]
  (concat [ (update-in (first s) [:word] clojure.string/capitalize)]
          (map #(cond
                  (= (:pos %) "NNP") %
                  :else (update-in % [:word] clojure.string/lower-case)
                  ) (rest s) )
          [ (inspair ".")]
          )
  )

(defn sentence-string
  "Given a single sentence in POS words make a sentence string with appropirate spaces"
  [s]
  (let [spaces (concat [ "" ] (rest  (map #(cond
                                             (= (:pos %) "POS") ""
                                             (= (:pos %) "RB") ""
                                             (= (:pos %) ".") ""
                                             (= (:pos %) ",") ""
                                             :else " "
                                             )
                                          
                                          
                                          s)) )]
    (clojure.string/join "" (interleave  spaces (map :word s))))
  )

(defn multi-nnp-fix
  "If the config has an NNP substitution and the NNP appears multiple times, voila"
  [config s]
  (if (not (contains? config :NNP))
    s
    (let [nnp (:NNP config)
          ;; ERROR CONDITION for nnp len > 1
          _   (if (> (count nnp) 1) (throw (Exception. "Only handle NNP size 1 for now")))
          ns (first nnp)
          name (first ns)
          subs (second ns)
          sub-to-obj  {"he" "him" "she" "her" "it" "it" "they" "them"}
          ]
      (loop [sent s
             seen-first false
             result []
             ]
        (if (empty? sent)
          result
          (let [fs   (first sent)
                rs   (rest sent)
                isnnp (= (:word fs) name)
                next-seen-first (or seen-first isnnp)
                usefs (if (and isnnp seen-first)
                        (cond
                          (not (nil? (:dobj (:grammar fs)))) (assoc fs :word (get sub-to-obj subs))
                          :else (assoc fs :word subs))
                        fs)
                ]
            (recur rs next-seen-first (conj result usefs)))
          )
        )))
  )

;; OK so lets start with some cause and effect
(defn combine-cause
  "Given two sentences, where A is caused by cause, combine using because and so and so on"
  ([a cause] (combine-cause a cause {}))
  ([a cause config]
   (let [ss_a         (sentence-structure a) ;; For now assume they are punctuated
         ss_cause     (sentence-structure cause)

         pos_a        (drop-last (:sent ss_a))
         pos_cause    (drop-last (:sent ss_cause))
         
         good      (->>  [
                          (concat pos_a [(insel "because")] pos_cause)
                          (concat pos_cause [(inspair ",") (insel "so")] pos_a)
                          (concat [(insel "because")] pos_cause [(inspair ",")] pos_a)
                          ]
                         (map case-fix)
                         (map (partial  multi-nnp-fix config))
                         (map sentence-string)
                         )
         
         ]
     good
     )
   )
  )

(def combine-samples
  [["We were sad." "Jim's dog died last week."]
   ["Hillary didn't become president." "Hillary lost Michigan." {:NNP [["Hillary" "she"]]}]
   ["Frank walked to school." "Frank lost his bike." {:NNP [["Frank" "he"]] }]
   ["Frank was scared of dogs." "A dog attacked Frank." {:NNP [["Frank" "he"]]}]
   ]
  )

(let [res   (map #(apply combine-cause %) combine-samples)]
  (print res)
  res
  )
#_(multi-nnp-fix (last (nth combine-samples 2)) (first  (apply combine-cause (nth combine-samples 2))))


(apply combine-cause (last combine-samples))

#_(map :dep  [(sentence-structure "A dog attacked Frank.")
              (sentence-structure "Frank was scared of dogs.")])


#_(:sent  (sentence-structure "A dog attacked Frank."))

#_ (:foo #{:foof :bar})
