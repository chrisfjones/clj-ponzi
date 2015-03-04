(defproject clj-ponzi "0.1.0"
  :description "Datomic helpers"
  :url "http://github.com/chrisfjones/clj-ponzi"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.datomic/datomic-free "0.9.5130" :exclusions [joda-time]]])
