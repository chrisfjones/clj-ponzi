(ns clj-ponzi.core-test
  (:require [datomic.api :as d])
  (:use [clojure.test]
	[clj-ponzi.core])
  (:import [datomic.db.Db]))

(deftest test-attr
  (is (= 1 1)))
