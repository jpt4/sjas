(ns lcert.extracted-runtime-test
  "ADR-0008: the generated core (src/lcert/extracted/runtime.clj) runs on
  plain Clojure, with no Ansatz on the classpath.  The values below are the
  generator's runtime representation of kernel terms; the formal suite's
  standalone-runtime-source test checks the same verdicts by kernel
  reduction (rfl) and checks that this file's source is up to date."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.extracted.runtime :as rt]))

;; Exp and DT values: a tagged vector [constructor-index field…]; List.nil is
;; nil.  Exp.star, Exp.tUnit, Exp.tBool:
(def ^:private star [12])
(def ^:private unit [1])
(def ^:private bool [2])
;; DT.rConst [] [] ⋆ Unit, and DT.fBase [] Unit.
(def ^:private rconst [31 nil nil star unit])
(def ^:private fbase [0 nil unit])

;; encCert 0 ⋆ Unit rconst fbase: the canonical F7 certificate that ⋆ : Unit
;; at budget 0, as a Code tree ([0 l] = sl l, [1 l a b] = sn l a b).
(def ^:private cert
  [1 96 [1 0 [1 3 [1 3 [1 3 [0 2] [0 0]] [0 0]] [0 0]]
         [1 0 [0 2] [1 0 [0 30] [1 0 [0 16]
           [1 0 [1 31 [0 0] [1 0 [0 0] [1 0 [0 30] [1 0 [0 16] [0 0]]]]]
                [1 0 [1 0 [0 0] [1 0 [0 16] [0 0]]] [0 0]]]]]]]
   [0 0]])

(deftest canonical-f7-certificate
  (testing "the extracted encoders build the certificate and the type code"
    (is (= cert (rt/encCert 0 star unit rconst fbase)))
    (is (= [0 16] (rt/encE unit))))
  (testing "the extracted checker decides it as the kernel does"
    (is (some? (rt/decCert cert)))
    (is (true? (rt/Check rt/decCert cert (rt/encE unit))))
    (is (false? (rt/Check rt/decCert cert (rt/encE bool)))))
  (testing "a certificate with a changed label decodes to nothing and is rejected"
    (is (nil? (rt/decCert (assoc cert 1 0))))
    (is (false? (rt/Check rt/decCert (assoc cert 1 0) (rt/encE unit))))))
