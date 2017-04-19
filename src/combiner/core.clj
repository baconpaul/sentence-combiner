(ns combiner.core
  (:import (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
           (edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation
                                  CoreAnnotations$TokensAnnotation
                                  CoreAnnotations$TextAnnotation
                                  CoreAnnotations$PartOfSpeechAnnotation
                                  CoreAnnotations$NamedEntityTagAnnotation
                                  )
           (edu.stanford.nlp.trees TreeCoreAnnotations$TreeAnnotation)
           )
  )

(defn new-pipeline []
  (let [p       (doto  (java.util.Properties.)
                  (.setProperty "annotators" "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
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
        
        res {:sent  (map tok_to_map toks)
             :tree  (.get sent TreeCoreAnnotations$TreeAnnotation)
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
  (loop [sent s
         seen-first false
         result []
         ]
    (if (empty? sent)
      result
      (recur (rest sent) seen-first (concat result [(first sent)]))
      )
    )
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
                         #_(map (partial  multi-nnp-fix config))
                         #_(map sentence-string)
                         )
         
         ]
     good
     )
   )
  )

(def combine-samples
  [["We were sad." "Jim's dog died last week."]
   ["Hillary didn't become president." "Hillary lost Michigan."]
   ["Frank walked to school." "Frank lost his bike." {:NNP { "Frank", "he" }}]

   ;; This one doesn't work with NNP substitution yet
   ["Frank was scared of dogs." "A dog bit Frank when he was small."]
   ]
  )

#_(let [res   (map #(apply combine-cause %) combine-samples)]
    (print res)
    res
    )
(multi-nnp-fix (last (nth combine-samples 2)) (first  (apply combine-cause (nth combine-samples 2))))

