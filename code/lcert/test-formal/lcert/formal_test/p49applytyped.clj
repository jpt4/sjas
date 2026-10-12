(ns lcert.formal-test.p49applytyped
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49applytyped]))

(deftest four-recursive-applications
  (is (b/has? 'p49at_evidence_subst))
  (is (b/has? 'p49at_apply_pair))
  (is (b/has? 'p49at_apply_four)))
