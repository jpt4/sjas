(ns lcert.formal-test.p49telenat
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49telenat]))

(deftest pointwise-telescope-environments
  (is (b/has? 'p49plug_rho_congr))
  (is (b/has? 'p49tele_rho_congr)))
