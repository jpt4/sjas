(ns lcert.formal.sem
  "F3d — semantic types (R4-metatheory.md §3.3).

  V chkf dec encTy n A G η k s v: at cap n, the value v : Car s lies in
  Vⁿₖ(A)η.  The type A is interpreted in the skeleton context G with the
  environment η.  The clauses (generated into sem_gen.clj by
  formal/tools/gen_sem.py) transcribe §3.3:
    V(0) = ∅; V(1), V(Bool), V(Nat), V(Syn) everything; V(Lbl) the labels;
    Vₖ(◇) = {◇} if k ≥ 1; Vₖ(R) the trees of at most k internal nodes;
    V(T(b))η = {⋆} if ⟦b⟧ⁿη = tt;
    Π at 0: every argument in the carrier; at 1: every j with k + j ≤ n and
    argument in Vⱼ, the result in Vₖ₊ⱼ; at ω: arguments in V₀;
    Σ at 0: the second component; at 1: the footprint k split as j + (k − j);
    at ω: the first in V₀;
    the branch-list pseudo-type: every branch, for labels from k₀ to 99.
  Where the skeleton s does not have the shape the type requires, False."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]))

(load "sem_gen")
