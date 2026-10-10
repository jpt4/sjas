(ns lcert.formal-test.p49teleformbridge
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49teleformbridge]))

(deftest selected-hole-telescope-formation
  (is (b/has? 'p49tf_rho0_typed))
  (is (b/has? 'p49tf_selected_env_typed))
  (is (b/has? 'p49tf_F_closed_formed)))
