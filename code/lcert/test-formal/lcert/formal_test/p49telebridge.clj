(ns lcert.formal-test.p49telebridge
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49telebridge]))

(deftest singleton-telescope-is-proposition
  (is (b/has? 'p49tele_P49))
  (is (b/has? 'p49tele_P49_closedenv)))
