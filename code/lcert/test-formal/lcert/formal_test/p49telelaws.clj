(ns lcert.formal-test.p49telelaws
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49telelaws]))

(deftest terminal-telescope-equivariance
  (is (b/has? 'p49tl_choose_lift))
  (is (b/has? 'p49tl_choose_subst))
  (is (b/has? 'p49tl_lift_nil))
  (is (b/has? 'p49tl_subst_nil))
  (is (b/has? 'p49tl_lift_nil_closed))
  (is (b/has? 'p49tl_subst_nil_closed))
  (is (b/has? 'p49tl_frame_lift))
  (is (b/has? 'p49tl_frame_subst))
  (is (b/has? 'p49tl_step_lift_fun))
  (is (b/has? 'p49tl_step_subst_fun))
  (is (b/has? 'p49tl_lift_equiv))
  (is (b/has? 'p49tl_subst_equiv))
  (is (b/has? 'p49tl_lift_F_closed))
  (is (b/has? 'p49tl_subst_F_closed))
  (is (b/has? 'p49tl_lift_F_stable))
  (is (b/has? 'p49tl_subst_F_stable))
  (is (b/has? 'p49tl_p49dl_BLift))
  (is (b/has? 'p49tl_p49dl_BSubst))
  (is (b/has? 'p49tl_p49dl_Lift))
  (is (b/has? 'p49tl_p49dl_Subst)))
