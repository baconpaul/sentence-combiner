(ns combiner.sentence-manipulator
  (:require [clojure.walk :as w])
  )

(defn insert-el [s] { :word s :pos "INSERTED" :ner "O"})
(defn insert-pair [s] { :word s :pos s :ner "O"})

(def comma (insert-pair ","))
(def period (insert-pair "."))


(defn case-fix
  "Given a collection of POS tagged words perhaps with manual inserts, fix the capitalization.
  We assume that we are a single sentence, and for now assume POS as 'NNP' is the indicator of proper noun (but should use ner later)"
  [s]
  (let [capitalized (concat [ (update-in (first s) [:word] clojure.string/capitalize)]
                            (map #(cond
                                    (and  (= (:pos %) "NNP") (not (= (:ner %) "O"))) %
                                    :else (update-in % [:word] clojure.string/lower-case)
                                    ) (rest s) )
                            )
        final       (if (= (:pos (last capitalized)) ".")
                      capitalized
                      (concat capitalized [period])
                      )
        ]
    final)
  )

(defn sentence-string
  "Given a single sentence in POS words make a sentence string with appropirate spaces"
  [s]
  (let [spaces (concat [ "" ] (rest  (map #(cond
                                             (= (:pos %) "POS") ""
                                             (and  (= (:pos %) "RB") (.contains (:word %) "'")) ""
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

(defn omit-each-punct
  "Given a structured sentence, for each bit of punctuation return a new sentence with it stripped"
  [insent]
  (loop [s insent
         p []
         r []]
    (if (empty? s)
      r
      (let [h  (first s)
            t  (rest s)
            rn (if (or  (= (:pos h) ".")
                        (= (:pos h) ",")
                        )
                 (conj r (concat p t))
                 r
                 )
            pn (conj p h)
            ]
        (recur t pn rn)
        ))
    )
  )

(defn find-in-tree
  "Given a structured sentence and a filter operator find the elements matching the filter"
  [ s op ]
  (w/walk
   (fn [v]  (if (op v) v nil ) )
   #(filter (comp not nil?) %) 
   (tree-seq (comp not nil?) (fn [v] (.getChildrenAsList v)) (:tree s))   )
  )

(defn find-label-in-tree
  "Given a structured sentence and a label find the nodes with that label"
  [s lab]
  (find-in-tree s #(= (.toString (.label %)) lab))
  )

(defn tree-terminal-strings
  [t]
  (w/walk
   (fn [v]  (if (empty? (.getChildrenAsList v)) (.value v) nil))
   #(filter (comp not nil?) %)
   (tree-seq (comp not nil?) (fn [v] (.getChildrenAsList v)) t))
  )


