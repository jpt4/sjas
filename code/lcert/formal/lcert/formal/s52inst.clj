(ns lcert.formal.s52inst
  "Theorem 5.2: motive instances for RecSyn (R4 §5).

  These seven proofs repeat only equality transport from recsyn.clj:
  leafTy/nodeTy read a method's result as S(P) at its code; y1Ty/y2Ty and
  their concrete-value versions insert recursive results into the node
  environment; node_raw transports the two accumulator skeletons back to
  skel P. They use S's substitution theorem, never a premise about V.

  The generator reads the existing equality proofs and substitutes S52 for
  V, removing the footprint and adding one fixed runtime value. Every
  generated statement and proof is elaborated and checked afresh. This
  keeps the de Bruijn substitutions and dependent equality paths in sync
  with their established definitions without changing those definitions."
  (:require [ansatz.core :as a]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [lv]]
            [lcert.formal.s52env]
            [lcert.formal.recsyn]))

(def ^:private names
  '{V_leafTy s52_leafTy, V_nodeTy s52_nodeTy,
    V_y1Ty s52_y1Ty, V_y2Ty s52_y2Ty,
    V_y1_val s52_y1_val, V_y2_val s52_y2_val,
    node_v_raw s52_node_raw})

(defn- relation-proof [form]
  (walk/postwalk
    (fn [x]
      (cond
        (= x 'n) 'cap
        (= x '(skels D)) 'G
        (and (seq? x) (= 'V (first x)))
        (let [[_ chk dec enc cap A G en _foot s a] x]
          (list 'S52 chk dec enc cap 'erasing A G en s 'rv a))
        (and (seq? x) (= 'V_subst (first x)))
        (let [[_ chk dec enc cap Gp A der G sg en hs _foot a] x]
          (list 's52_subst chk dec enc cap 'erasing Gp A der G sg en hs 'rv a))
        :else x)) form))

(with-open [r (java.io.PushbackReader. (io/reader (io/resource "lcert/formal/recsyn.clj")))]
  (loop []
    (when-let [form (read {:eof nil} r)]
      (when (and (= 'thm (first form)) (names (second form)))
        (let [[_ old params goal & tactics] (relation-proof form)
              params (vec (mapcat
                (fn [[p sep ty]]
                  (cond (= p 'K) []
                        (= p 'cap) [p sep ty 'erasing :- 'Bool 'rv :- 'RV]
                        (= p 'D) '[G :- (List Sk)]
                        :else [p sep ty])) (partition 3 params)))]
          (a/prove-theorem (names old) (lv params) (lv goal) (lv (vec tactics)))))
      (recur))))
