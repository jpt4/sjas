# Proposition 4.9, general case — design note

Branch `f4-prop49` (from `f6-p5`, 0aa1f92). Written 2026-10-08, before the proofs.
Final proof status is in §12 and ADR-0006; §§9–11 record earlier stopping points.

## 0. What is to be proved

For every `k`, a closed budget-0 term `t49 k` with

    Rt chkf nil nil (t49 k) (P49 k)
    P49 k = Π(c :ω Syn). Π(e :1 T(dl k c)). Π(e′ :1 T(chk′ c c⊥)). 0

for every `chkf` that meets `CheckSpec` (with `dec`, `encTy`; Corollary 3.7 is
the only use of the hypotheses), and `prop49_concrete` at `Check decCert`.

**`depthLeq` is fixed per k.** The paper says "define `depthLeq` by `recSyn`"
and states the proposition for each `k`. We define `dl j e` by recursion on the
*meta-level* number `j`, each level being one `recSyn`:

    dl 0 e       = recSyn[Bool] tt ff e
    dl (j+1) e   = recSyn[Bool] tt (ite (dl j c1) (dl j c2) ff) e      (c1, c2 the subcodes)

so `T(depthLeq c k)` in P49 k is `T(dl k c)`: "c has depth ≤ k" (leaf depth 0).
At `k = 0` this is exactly `sh4_depth0` of `section4c.clj` (checked by a suite
test). We do **not** also prove the statement for a single `depthLeq` with a
`Nat` argument and `recN`; that would only add a conversion chain from the
unfolded form, and the paper's own term is per `k`.

## 1. Why the obvious induction fails, and the generalised statement

`chk′ c c⊥` δ-reduces only when `c` is a closed canonical code (conv.clj,
`Hd.delta`), and `chkf` is a generic function: its value on a node is not a
function of its value on the children. So `T(chk′ c c⊥)` becomes `T(ff)` only
after the case analysis has fixed *every* label and every subcode of `c`. The
case tree therefore has doubly exponentially many leaves in `k`
(100 labels per node, two subtrees per node) and the term cannot be built
concretely, even for `k = 1` (about 100³ codes).

Generalised statement. A **code context** `K` is a tree

    K ::= hole | kleaf l | knode l K K            (l < NL = 100)

Its holes, read left to right, are quantified in that order, each with its own
depth bound and its own evidence. For a list of bounds `js` (one per hole,
`|js| = holes K`):

    tele []      K = Π(e′ :1 T(chk′ ⟦K⟧ c⊥)). 0
    tele (j::js) K = Π(h :ω Syn). Π(e :1 T(dl j h)). lift 1 0 (tele js K)

where `⟦K⟧` replaces `kleaf l` by `sleaf (lbl l)`, `knode l a b` by
`snode (lbl l) ⟦a⟧ ⟦b⟧`, and the hole with `r` holes to its right by `var r`.
The `lift 1 0` makes the evidence binder invisible to the tail (the evidence is
non-dependent); the net index of that hole at the base is `2r+1`.
`tele (bnds K) K` is a *closed* type, and `tele [k] hole = P49 k`.

**Induction statement** (fuel `f`, potential `pot js = Σ 3^j`):

    ∀ f K js, |js| = holes K → pot js ≤ f →
      ∀ D us, lenU us = lenE D →  Rt chkf D us (τ f K js) (tele js K)

`τ f K js` is a closed term, so the statement quantifies over every context and
every usage vector (this is how the forced usages `y₁,y₂ : 1` of recSyn's node
branch are met without a usage-weakening lemma: the term is derivable at any
vector, and the vector is carried by one closed sub-term).

## 2. The generator

`τ f K js` peels the first hole by one application of the **peel lemma H**:

    H (j, W₀, L, N)  :  Π(c :ω Syn). Π(e :1 T(dl j c)). lift 1 0 W₀

where `W₀ = tele js′ K` is unary (only variable 0 free), `L l` is a closed term
for each `l < NL` of type `W₀[sleaf (lbl l)]`, and (when `j ≥ 1`)
`N l` is a closed term for each `l` of type

    Π(c₁ :ω Syn). Π(e₁ :1 T(dl (j−1) c₁)). Π(c₂ :ω Syn). Π(e₂ :1 T(dl (j−1) c₂)). W₀[snode (lbl l) c₁ c₂].

The term is `λc. recSyn[M] (tl) (tn) c` with motive `M(x) = Π(e :1 T(dl j x)). lift 1 0 W₀`;
`tl = caseLbl a (λe. L l)_l`, `tn = caseLbl a (λe. Split…)_l`. In the node branch
the evidence `e : T(dl j (snode l c₁ c₂))` converts (one ι-step, `Hd.recSN`,
inside `T`) to `T(ite (dl (j−1) c₁) (dl (j−1) c₂) ff)`; the **Split lemma** (an
`elimBool` on the first conjunct, motive `T(ite x X ff) ⊸ T(x) ⊗ T(X)`) turns it
into a pair, a `let` unpacks it, and `N l c₁ x c₂ y` finishes. This is the
paper's "threading the evidence through `elimBool` on `&&`". At `j = 0` the node
branch is dead: `T(dl 0 (snode …)) ▹ T(ff) ▹ 0` (Hd.recSN gives `ff`; Hd.tTF),
and the branch is `abort`.

`τ f K js` for `js = j::js′`:
* `L l := τ (f−1) (K[hole := kleaf l]) js′`,
* `N l := τ (f−1) (K[hole := knode l hole hole]) ((j−1)::(j−1)::js′)`
  (the two new holes replace the first hole, which is why `pot` decreases:
  `3^j > 2·3^(j−1)`),
* for `js = []`: `K` is canonical and `τ f K [] = λ(e′ :1 T(chk′ ⟦K⟧ c⊥)). e′`
  at type `0` by a δ-conversion (§4).

`t49 k := τ (3^k) hole [k]`.

## 3. Type equalities (the algebra)

The hypotheses of H are stated for `W₀[…]`, the induction supplies `tele js K′`.
The bridge, the only place where substitution is computed, is

    (Q)  subst θ_p (tele js K) = tele js (K[first hole := S])

for `S ∈ {kleaf l, knode l hole hole}`, by induction on `js` generalising the
number `p ≥ 1` of holes already peeled; `θ_p` sends the oldest peeled variable
`p−1` to `⟦S⟧` (its new holes at `var p`, `var (p−1)`) and shifts the others.
Three facts make this go: `upn 1 θ_p = θ_{p+1}` (pointwise); `subst (upn 2 σ) (lift 1 0 X) =
lift 1 0 (subst (upn 1 σ) X)` (`lift_subst`, derivations.clj); at the base,
`subst σ ⟦K⟧ = ⟦K⟧` computed hole by hole, and `⟦K[first := S]⟧` equals `⟦K⟧`
with the first hole's reference replaced by `⟦S⟧` (a lemma on `K` by induction,
splitting on whether the left child has a hole). `subst σ (dl j e) = dl j (subst σ e)`
because `dl`'s fixed parts only mention variables bound inside it.

## 4. δ-steps at canonical codes

Corollary 3.7(i) (`cor37_refutation`, lemma36.clj) gives `chkf cc (encTy tEmpty) =
false` for **every** code `cc`, from `CheckSpec` and `conv_all`. At a canonical
`K`, `codeOf ⟦K⟧ = some cc` (induction on `K`; labels `< NL`), and
`codeOf c⊥ = some (encTy tEmpty)` by `CheckSpec`'s base-code clause. So
`Hd.delta` gives `chk′ ⟦K⟧ c⊥ ⇝ ff`, then `Hd.tTF`: `T(chk′ ⟦K⟧ c⊥) ▹ T(ff) ▹ 0`.
This is one `Cv` chain of two steps (cf. `sh4_cv_ff`), generic in `cc`.

## 5. Conversion chains

Always by a lemma per rule, never concretely:
* `T(dl j (sleaf x)) ▹ T(tt)` is not needed (leaf evidence is unused);
* `T(dl j (snode x c₁ c₂)) ▹ T(ite (dl (j−1) c₁) (dl (j−1) c₂) ff)`: `Step` at
  path `[0]` with head `Hd.recSN`, then `substL` computed on the closed body;
* `T(dl 0 (snode …)) ▹ T(ff) ▹ 0`: `Hd.recSN` then `Hd.tTF`;
* the three-step δ chain of §4.
Every chain element needs `SkJ Bool.true G · Sk.unit` and `nbr = true`; these
come from one lemma per shape of `dl`/`chk′`/`T` term.

## 6. The 100-way case split

`caseLbl` with 100 branches is typed through `tBrs P k`. One lemma by downward
induction on `k` (`rt_brs`): if `f l` types at `P[lbl l]` for every `l < NL`, then
the chain `bcons (f k) (bcons (f (k+1)) …)` types at `tBrs P k`. The branch family
`l ↦ τ(…K[…l…]…)` is a single function of `l`, so the lemma is applied once.

## 7. Usages

`τ` is derivable at *every* usage vector of the right length (the scrutinee
`var 0` carries the whole vector; the other premises are at the zero vector,
`zero_vadd`, `zero_vscale`). Recursive results `y₁,y₂` of recSyn are unused
(forced usage `1` is met by the vector carried by the closed sub-term).

## 8. Plan and failure criterion

1. `k = 0` complete and concrete (no tele/K machinery): `prop49_0`.
2. Algebra (Q, `dl` substitution, closedness) and formation of the fragment.
3. Peel lemma H, Split lemma, `rt_brs`.
4. The generator and the induction; `prop49`, `prop49_concrete`.
If step 2 or 3 cannot be completed at reasonable cost, deliver step 1 (and
`k = 1` if feasible) and record the precise obstruction here and in ADR-0006.

## 9. Status

**Current status, 2026-10-12:** General Proposition 4.9 is proved for every
depth. `p49all.clj` defines `t49 k` and kernel-checks `prop49` under
`CheckSpec` and `prop49_concrete` at `Check decCert`, each at budget zero.
See §12 for the proof and validation. The following records the earlier
2026-10-08 fallback and its stopping point; it has been superseded.

2026-10-08 historical status: fallback delivered, with the general case then open.

* `p49zero.clj`: `prop49_0` proves `Rt chkf nil nil p49z_term P49zero`
  from `CheckSpec chkf dec encTy`. `prop49_0_concrete` has no hypotheses
  at `Check decCert`. This completes k = 0, including the 100-way label
  split and the dead node arm. `p49z_depth_agrees` identifies the bound
  with `sh4_depth0`. The initial uncommitted implementation was recovered,
  read, checked in the existing server, and committed as `25ea8dd`.
* `p49depth.clj`: the kernel functions `p49dl : Nat → Exp → Exp` and
  `P49 : Nat → Exp` implement §0. `p49dl_subst` is proved by induction on
  k, generalized over every substitution and argument. `p49dl_leaf`,
  `p49dl_zero_node`, and `p49dl_node` certify the rule-sized head steps.
  `p49dl_typed` proves type-level Bool typing at every k in every context
  with a typed Syn argument; `P49_formed` proves formation at every k.
  `P49_zero_agrees` and `prop49_zero` give the depth-zero result in the
  general family's notation. Committed as `cd4958c`.
* At that point there was no `t49`, `prop49`, or `prop49_concrete`, and no
  positive-depth inhabitant had been proved. Formation was not inhabitation.

### Exact stopping point and cost boundary

The algebra for `dl` in step 2 is complete, but the context/telescope
algebra (Q) and step 3 are not. In particular, `p49z_leaf_arm` has the
fixed conclusion `P49zero` specialized to a leaf, and `p49z_node_arm`
closes by the depth-zero bound's reduction to ff. They are not instances
of a proved peel lemma quantified over a code context or continuation.
For a positive bound, the node arm reduces instead to
`T(ite (p49dl j c1) (p49dl j c2) ff)`; this is not convertible to 0 for
open c1,c2. It needs the dependent Boolean split plus recursively typed
continuations, whose final checker argument contains the *entire rebuilt
code*, not either child. Applying `prop49_0` to a child proves the wrong
checker-evidence type. Corollary 3.7 is a metatheorem and does not permit
an object-level delta step at that open rebuilt code.

To bridge this gap, the remaining kernel work is:

1. Define well-scoped code contexts, hole replacement, their interpretation
   at arbitrary binder offsets, and `tele`; prove closedness and formation.
2. Prove (Q) with the precise index maps, including the one-hole-to-two-holes
   replacement. Replacing the first hole changes the arity and the indices
   of the remaining holes. Existing expression substitution laws alone
   do not identify those two generated telescopes.
3. Prove Split and peel H with formation, usage-vector and conversion
   premises, and a branch-list construction quantified over a label family.
4. Define the fuel recursion inside the kernel and prove typing with the
   potential decrease. A Clojure function expanding concrete k is not this
   theorem.

This is an unfinished proof-construction boundary, **not** a counterexample
or a demonstrated limitation of Ansatz. The generalized induction was not
formalized within this bounded attempt at reasonable cost: it requires the
new indexed-context infrastructure and the typed combinators above, beyond
extending the fixed-context builders. The task's explicit fallback is used
rather than marking the schematic induction as proved. k = 1 is also not
claimed: enumerating its roughly one million canonical nodes is excluded
by the task and would not discharge the missing general induction.

### Deviations and validation

* Step 1 uses small Clojure builders for explicit rule proofs and a fixed
  100-branch list. These emit kernel-checked declarations only; this is
  permitted concrete depth-zero work, not the proposed all-k generator.
  `dtgen.clj` from f63 was neither copied nor used.
* The node arm at depth zero returns the depth evidence directly after
  conversion to 0; no abort wrapper is needed when the target itself is 0.
* Delta certification is `p49z_delta`, generic in a canonical expression
  and its `codeOf` certificate. `p49z_checker_empty` connects CheckSpec's
  encoding of 0 to literal code 15; `cor37_refutation` with `conv_all`
  supplies the false equation. The conversion has **two** forward steps,
  one under T and one T(ff) → 0 (correcting §5's “three-step” wording).
* The `recSyn` list substitution uses `substL_instLS`; `instL` is
  well-founded and cannot be evaluated by reflexivity at symbolic fields.
  Direct rewrites left an incomplete goal; explicit congruence chains
  plus this existing structural-lookup lemma finished `p49dl_node`.
* No existing formal definition or rule was changed. General statements
  were not silently weakened: the proved inhabitant names carry zero.
* Warm suites: p49zero, 9 assertions; p49depth, 10 assertions; both passed.
  p49depth's tests were first run red (missing theorems), then green.
  Its negatives reject identifying successor depth syntax with depth zero;
  p49zero rejects using the bounded theorem at the unbounded type and
  an incorrect depth-zero node head certificate. These are rejection tests
  of the supplied proof attempts, not theorems of non-inhabitability.
* Fresh validation: `bin/test-formal p49zero p49depth` passed after stopping
  the sole server: **2 tests, 19 assertions; 0 failures, 0 errors; status=0**.
  Durable log: `code/lcert/test-runs/p49-fresh.log`, finished
  2026-10-08T20:36:32Z, elapsed 49:21.04, maximum RSS 1330104 KB.
  The resumed session verified the result and unchanged formal sources;
  `bin/check-formal-names` also passed. No full suite was run.
* The supervisor's `p49c.done` marker is not written: its explicit condition
  requires every deliverable to be proved, whereas this is the authorized
  fallback with the general theorem still open.

## 10. Renewed assessment (2026-10-08)

The requested target remains the all-depth `Rt` theorem, or a refutation;
the earlier fallback does not meet that target. A possible simplification
is bounded dependent elimination for an arbitrary formed motive `P(c)`:
canonical cases of depth at most k should suffice to construct
`Π(c :ω Syn). T(dl k c) ⊸ P(c)`. In the node case instantiate the induction
hypothesis twice, retaining the other child in the motive. This may avoid
the code-context telescope, but requires explicit substitution, formation,
and usage proofs; it is not yet a proved lemma.

First isolate the required Boolean operation in `p49split.clj`:
`Π(b :ω Bool). Π(c :ω Bool). T(ite b c ff) ⊸ (T(b) ⊗ T(c))`.
Its true arm returns `(⋆, e)` after conversion; its false arm eliminates
`e : T(ff)`. The single affine evidence is never duplicated. The new
namespace deliberately depends only on the rule table, not consistency
or the checker. Proving this auxiliary does not establish Proposition 4.9.

## 11. Resumed proof work (2026-10-09)

The current worktree adds kernel-checked auxiliary files for Boolean evidence
splitting (`p49split`), named-hole context substitution (`p49context`),
context typing and code readback (`p49contextfacts`), the 100-way branch
family (`p49branches`), a generic peel with explicit premises (`p49peel`),
and a telescope with one- and two-hole indexing checks (`p49tele`). The
successor-depth peel file `p49peeldepth` proves runtime depth computation,
the node evidence splitter in its actual runtime context, and an assembled
`p49pd_peel` from precisely stated leaf and node continuation premises in a
warm REPL run. The earlier combined fresh suite failed with a Clojure `d.mk`
class-loading error before it could certify the peel. The generic telescope
was isolated from the depth proof and passes its fresh focused suite
(1 test, 3 assertions). The singleton identity `p49tele_P49` moved to
`p49telebridge`; its separate cold suite passed (1 test, 2 assertions,
status 0; `code/lcert/test-runs/p49-bridge-fresh-20261009.log`). The
successor-depth peel still needs its own clean cold focused result.

The telescope replacement identity is now supplied below, but the
arity-changing peel bridge and a kernel-defined fuel recursion producing
closed leaf and node continuations are still missing.
In particular, `p49pd_peel` has two `Rt` premises, so it is not `prop49`.
The all-depth inhabitant and any refutation remain open.

The next assembly obligations are now sharper: instantiate the successor peel
with the telescope tail as its unary family, prove lift/substitution laws for
that concrete family, and construct the typed leaf/node continuations from
the resulting tails. The arbitrary-context depth-zero peel now exists; its
node arm consumes the impossible depth evidence. The fuel induction must
still construct the continuation premises of each peel.

The kernel now checks the strict fuel inequalities in `p49fuel.clj`:
`p49pow3_pos`, `p49pot_leaf_decrease`, and `p49pot_node_decrease` cover
the two generator transitions for a list of named holes. The fresh focused
suite passed (1 test, 4 assertions, status 0; log
`code/lcert/test-runs/p49-fuel-focused-20261009.log`). These inequalities
establish termination of a proposed recursion, not the recursion or its
`Rt` invariant.

The new `p49fresh.clj` proves that its additive bound exceeds every named
hole in both the context and the pending slot list (fresh focused suite:
1 test, 5 assertions, status 0). `p49telenat.clj` proves pointwise
environment congruence for the generic telescope (fresh focused suite:
1 test, 2 assertions, status 0). Neither theorem constructs a continuation.
`p49teleform.clj` now has a cold passing formation proof (1 test, 3
assertions, status 0; `code/lcert/test-runs/p49-teleform-v2-20261009.log`).
The arbitrary-context depth-zero peel in `p49peelzero.clj` also has a cold
passing run (2 tests, 12 assertions, status 0;
`code/lcert/test-runs/p49-peelzero-fresh-20261009.log`). Both remain
conditional building blocks rather than the final inhabitant.

The telescope replacement obligation is now discharged separately in
`p49telerepl.clj`: `p49tele_repl` gives replacement for an arbitrary context
under explicit slot-avoidance premises, and `p49tele_fresh_node_repl`
specializes it to the two child holes selected by `p49fresh`. A fresh
focused run passed (1 test, 11 assertions, status 0; log
`code/lcert/test-runs/p49branches-telerepl-fresh-final.log`). This is the
context/environment equation, not the arity-changing bridge between the
peel continuation type and the telescope, nor the fuel-recursive `Rt` term.
The follow-on `p49genfacts.clj` certifies that both chosen child IDs avoid
the pending slots, including that the right child still avoids the list after
the left child is prepended (fresh focused run: 1 test, 5 assertions, status
0; `code/lcert/test-runs/p49genfacts-focused-v4-20261009.log`).

`p49telebridgeaux.clj` now identifies the instantiated leaf and node peel
result types with the corresponding replaced telescope tails. Its fresh
focused suite passes (1 test, 9 assertions, status 0;
`code/lcert/test-runs/p49branches-telebridgeaux-final.log`). These theorems
explicitly assume lift and substitution naturality for the selected unary
family; those laws have not been proved for the concrete telescope and need
closedness or equivariant environment hypotheses. The bridge does not yet
provide runtime continuations.

## 12. Final proof and validation (2026-10-12)

The all-depth theorem is complete. `p49all_fuel` proves the generalized
`telescope` runtime invariant by induction on finite fuel. The terminal case
closes an empty slot list. At a pending depth-zero slot, the leaf peel and
100 label continuations use the induction hypothesis at smaller potential.
At successor depth, the node peel uses two fresh child slots, the live
child code/evidence applications, and `p49pot_node_decrease`. The leaf
continuation's depth parameter is independent of its selected hole ID.

`prop49` yields `Rt chkf nil nil (t49 k) (P49 k)` for every `k : Nat` under
`CheckSpec chkf dec encTy`. `prop49_concrete` specializes this to
`Check decCert` without hypotheses. The exact closed statement is also
checked by `formal_test/p49all.clj`. The cold `bin/test-formal p49all` run
passed (1 test, 8 assertions, 0 failures/errors, 2026-10-12, log
`code/lcert/test-runs/p49-all-v21-20261012.log` in the P49 worktree).
Earlier open-status notes above are retained as dated process history.
