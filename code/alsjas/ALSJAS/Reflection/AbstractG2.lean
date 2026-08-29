/-!
# Coding-independent computational G2

This module contains no syntax codes, natural-number encodings, parser,
checker, or machine model. It states the exact proof-program interface needed
by the computational G2 construction and builds the complete proof term from
those operations.

The distinguished `godel` formula has only its selected fold and unfold maps;
the interface does not expose a uniform fixed-point generator. `copyBox` is
box-restricted contraction. Removing that field makes this construction
unavailable at precisely the duplicated boxed refutability value.
-/

universe u v

namespace ALSJAS.Reflection.AbstractG2

/- Abstract native proof-program calculus supporting the precise G2
construction. Programs are indexed by formulas, so every operation states its
proof type directly. -/
set_option linter.checkUnivs false in
structure Calculus where
  Formula : Type u
  Program : Formula → Type v
  zero : Formula
  tensor : Formula → Formula → Formula
  lolli : Formula → Formula → Formula
  box : Formula → Formula
  godel : Formula
  lam : {domain codomain : Formula} →
    (Program domain → Program codomain) → Program (lolli domain codomain)
  app : {domain codomain : Formula} →
    Program (lolli domain codomain) → Program domain → Program codomain
  splitTensor : {left right result : Formula} →
    Program (tensor left right) →
    (Program left → Program right → Program result) → Program result
  necessitate : {proposition : Formula} → Program proposition → Program (box proposition)
  boxCompose : {domain codomain : Formula} →
    Program (box (lolli domain codomain)) →
    Program (box domain) → Program (box codomain)
  introspect : {proposition : Formula} →
    Program (box proposition) → Program (box (box proposition))
  copyBox : {proposition : Formula} →
    Program (box proposition) → Program (tensor (box proposition) (box proposition))
  fixedFold : Program (box (lolli godel zero)) → Program godel
  fixedUnfold : Program godel → Program (box (lolli godel zero))

/-- Closed affine composition, used under necessitation in formalized G2. -/
def compose (calculus : Calculus) {domain middle codomain : calculus.Formula} :
    calculus.Program
      (calculus.lolli (calculus.lolli middle codomain)
        (calculus.lolli (calculus.lolli domain middle)
          (calculus.lolli domain codomain))) :=
  calculus.lam fun latter =>
    calculus.lam fun former =>
      calculus.lam fun input =>
        calculus.app latter (calculus.app former input)

/-- The Jeroslow step. Unfolding `G` gives one boxed refutation of `G`.
Box-restricted contraction supplies two copies: one is introspected and sent
through the necessitated fold map to obtain `box G`; the other consumes that
boxed `G`, yielding `box 0`. -/
def gToBoxZero (calculus : Calculus) :
    calculus.Program
      (calculus.lolli calculus.godel (calculus.box calculus.zero)) :=
  calculus.lam fun godelProof =>
    calculus.splitTensor (calculus.copyBox (calculus.fixedUnfold godelProof))
      fun firstRefutation secondRefutation =>
        let quotedFold := calculus.necessitate <|
          calculus.lam fun boxedNegatedGodel =>
            calculus.fixedFold boxedNegatedGodel
        let boxedGodel := calculus.boxCompose quotedFold
          (calculus.introspect firstRefutation)
        calculus.boxCompose secondRefutation boxedGodel

/-- Formalized G2: `box (box 0 -> 0) -> box 0`. The construction quotes
composition and the preceding closed Jeroslow map, folds the resulting boxed
negation into `G`, and applies the Jeroslow map once more. -/
def formalizedG2 (calculus : Calculus) :
    calculus.Program
      (calculus.lolli
        (calculus.box (calculus.lolli (calculus.box calculus.zero) calculus.zero))
        (calculus.box calculus.zero)) :=
  calculus.lam fun boxedSelf0 =>
    let quotedComposition := calculus.necessitate <|
      compose calculus
        (domain := calculus.godel)
        (middle := calculus.box calculus.zero)
        (codomain := calculus.zero)
    let quotedGToZero := calculus.necessitate (gToBoxZero calculus)
    let boxedAfterSelf0 := calculus.boxCompose quotedComposition boxedSelf0
    let boxedNegatedGodel := calculus.boxCompose boxedAfterSelf0 quotedGToZero
    let godelProof := calculus.fixedFold boxedNegatedGodel
    calculus.app (gToBoxZero calculus) godelProof

/-- Full coding-independent computational G2 proof term. A native `self0` is
necessitated, formalized G2 produces `box 0`, and `self0` consumes it. -/
def contradictionFromSelf0 (calculus : Calculus)
    (self0 : calculus.Program
      (calculus.lolli (calculus.box calculus.zero) calculus.zero)) :
    calculus.Program calculus.zero :=
  let boxedSelf0 := calculus.necessitate self0
  let boxedZero := calculus.app (formalizedG2 calculus) boxedSelf0
  calculus.app self0 boxedZero

/-- External consistency means that a closed contradiction program can be
eliminated into Lean's empty proposition. -/
abbrev Consistent (calculus : Calculus) : Prop :=
  calculus.Program calculus.zero → False

/-- G2 in exclusion form: if the calculus has no closed contradiction program,
then it has no closed program of its native consistency type. -/
theorem self0Excluded (calculus : Calculus)
    (consistent : Consistent calculus) :
    calculus.Program
      (calculus.lolli (calculus.box calculus.zero) calculus.zero) → False :=
  fun self0 => consistent (contradictionFromSelf0 calculus self0)

end ALSJAS.Reflection.AbstractG2
