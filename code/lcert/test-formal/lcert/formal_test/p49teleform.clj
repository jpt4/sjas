(ns lcert.formal-test.p49teleform
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49teleform]))

(deftest telescope-formation
  (is (b/has? 'p49tf_weaken2))
  (is (b/has? 'p49tf_update_typed))
  (is (b/has? 'p49tele_p49dl_formed)))
