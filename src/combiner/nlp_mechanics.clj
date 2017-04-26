(ns combiner.nlp-mechanics
  (:import (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
           (edu.stanford.nlp.ling CoreAnnotations$SentencesAnnotation
                                  CoreAnnotations$TokensAnnotation
                                  CoreAnnotations$TextAnnotation
                                  CoreAnnotations$PartOfSpeechAnnotation
                                  CoreAnnotations$NamedEntityTagAnnotation
                                  CoreAnnotations$LemmaAnnotation
                                  )
           (edu.stanford.nlp.trees TreeCoreAnnotations$TreeAnnotation)
           (edu.stanford.nlp.semgraph SemanticGraphCoreAnnotations$BasicDependenciesAnnotation)
           )

  )

(defn ^:private new-pipeline []
  (let [p       (doto  (java.util.Properties.)
                  (.setProperty "annotators" "tokenize, ssplit, pos, lemma, ner, parse, depparse, dcoref")
                  (.setProperty "ssplit.isOneSentence" "true")
                  )
        pl      (StanfordCoreNLP. p )
        ]
    pl
    ))

(defn ^:private annotate [txt]
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
                            :lemma (.get t CoreAnnotations$LemmaAnnotation)
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


