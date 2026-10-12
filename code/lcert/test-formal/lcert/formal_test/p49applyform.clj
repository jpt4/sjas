(ns lcert.formal-test.p49applyform
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49applyform]))

(deftest recursive-application-formation
  (is (b/has? 'p49af_first_env))
  (is (b/has? 'p49af_first_env_fun))
  (is (b/has? 'p49af_form)))
