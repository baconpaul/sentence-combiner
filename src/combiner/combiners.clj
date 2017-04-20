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
                        )
        soso      (->> [
                        (concat pos-cause [(sm/insert-el "and") ] pos-a)
                        ]
                       (map sm/case-fix)
                       (map sm/sentence-string))
        wrong     (->> [
                        (concat pos-cause [(sm/insert-el "because") ] pos-a)
                        (concat pos-cause [(sm/insert-el "although") ] pos-a)
                        ]
                       (map sm/case-fix)
                       (map sm/sentence-string)
                       )
        
        ]
    {:good  good
     :soso soso
     :wrong wrong}
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
                        )
        soso      (->> [
                        ]
                       (map sm/case-fix)
                       (map sm/sentence-string))
        wrong     (->> [
                        ]
                       (map sm/case-fix)
                       (map sm/sentence-string)
                       )
        
        ]
    {:good  good
     :soso soso
     :wrong wrong}
    )
  )
