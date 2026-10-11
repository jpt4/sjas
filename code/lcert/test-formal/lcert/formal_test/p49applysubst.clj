(ns lcert.formal-test.p49applysubst
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49applysubst]))

(deftest fresh-slot-binder-cancellation
  (is (b/has? 'p49as_linv))
  (is (b/has? 'p49as_cancel))
  (is (b/has? 'p49as_first_frame))
  (is (b/has? 'p49as_slot_env))
  (is (b/has? 'p49as_slot_env_fun))
  (is (b/has? 'p49as_slot_body))
  (is (b/has? 'p49as_pair_env))
  (is (b/has? 'p49as_second_slot_body)))
