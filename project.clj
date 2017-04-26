(defproject combiner "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [edu.stanford.nlp/stanford-corenlp "3.7.0"]
                 [edu.stanford.nlp/stanford-corenlp "3.7.0" :classifier "models"]
                 [net.sf.extjwnl/extjwnl "1.9.2"]
                 [net.sf.extjwnl/extjwnl-data-wn31 "1.2"]
                 ]
  :main combiner.core
  :aliases { "app" [ "run" "-m" "combiner.simple-app/make-simple-app"]}
  )
