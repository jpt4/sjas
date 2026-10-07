# Theorem 5.2, universal form — design note (ADR-0006, 2026-10-07)

Branch `f5-theorem52-universal`, from `f6-p5` at 17e51f7.  Files:
`code/lcert/formal/lcert/formal/s52*.clj` (existing), new files named in §3.

## 0. The obligation

The existential Theorem 5.2 (`theorem52`, `theorem52e`, `theorem52_concrete`)
says a derivable `Θₙ ⊢ t :¹ X` has *a* safe run.  R4 §5 says *the* run is
safe.  The evaluator is the relation `Ev` (eval.clj; `EvE`, erase.clj), which
is not deterministic, so the existential statement is strictly weaker.  The
required universal statements are

* `theorem52_all`: for derivable `Θₙ ⊢ t :¹ X`, for EVERY `v` and every `Ev`
  derivation of `Eval n (rtokens n) t v`, the run is `Ok` (no abort, H₁ or H
  node, nested reflect runs included);
* `theorem52e_all`: the same for `EvE`, for every erasure `e` of the
  derivation;
* `theorem52_all_concrete`: at `Check decCert`, with no hypotheses.

## 1. Audit of the nondeterminism of `Ev` and `EvE`

`Ev` is an inductive family indexed by `(n, src, v)`.  Source shapes are
`tm ρ e`, `ap f a`, `iter ρ st k acc`, `recs ρ tl tn c`, `itr g h c`.

For a source, the constructors that can conclude it:

| source | constructors | selected by |
|---|---|---|
| `tm ρ (var i)` | eVar | — (the lookup is a function) |
| `tm ρ star/tt/ff/zero/lbl l` | eStar eTT eFF eZero eLbl | — |
| `tm ρ (abort A t)` | eAbort | — (value `rdflt (skel A)`: a function) |
| `tm ρ (h1 …)` | eH1 | — (value `star`) |
| `tm ρ (ite b t e)`, `elimB` | eIteT/eIteF, eElimT/eElimF | the value of `b`: `bool true` vs `bool false` — disjoint |
| `tm ρ (succ m)` | eSucc | — |
| `tm ρ (recN …)` | eRecN, then `iter` | — |
| `iter ρ st k acc` | eIterZ (`k=0`), eIterS (`k=succ`) | the numeral — disjoint |
| `tm ρ (caseL P a bs)` | eCaseL | — |
| `tm ρ bnil` | eBnil | **the tag `s` is free: any skeleton** |
| `tm ρ (bcons h t)` | eBcons | — |
| `tm ρ (sleaf/snode/…)` | eSleaf eSnode | — |
| `recs ρ tl tn c` | eRecSL (`c = sl l`), eRecSN (`c = sn …`) | the code — disjoint |
| `tm ρ (leaf/node/itR/prn)` | eLeaf eNode eItR ePrn | — |
| `itr g h c` | eItL, eItN | the code — disjoint |
| `tm ρ (lam …)` | eLam | — |
| `tm ρ (app f u)` | eApp | — |
| `tm ρ (pair …)`, `letp` | ePair, eLet | — |
| `tm ρ (chk c d)` | eChk | — (value `chkf cc dc`: a function) |
| `tm ρ (refl D r e)` | eReflOk / eReflNo / eReflNone | `Bool.and (ble (cnodes c) n) (chkf c (encTy D))` false → No; true and `dec c = some …` → Ok; true and `dec c = none` → None — pairwise disjoint; in eReflOk `(m,t2,A)` is a function of `c` |
| `tm ρ (insp …)` | eInspT/eInspF | `chkf cv dv` — disjoint |
| `ap (clos ρ b) a` | apClos | — |
| `ap (bnil s) a` | **apBnil**: value `rdflt s` | the tag |
| `ap (bcons h t) (lbl 0)` / `(lbl (succ k))` | apBconsZ / apBconsS | the label — disjoint |

`EvE` has the same table with one further free choice: **eReflOk runs an
ERASURE `e2` of the decoded derivation** (`her : Er … t2 A e2`), and `Er` is a
relation, so `e2` is not determined.  (Everything else in `EvE` is as `Ev`.)

Defaults: `rdflt s` and `reifySk s` are total functions of the skeleton, so
`eAbort`, `eH1`, `eReflNo`, `eReflNone` determine their value.  `rdflt (arr x y)`
is a closure whose body is `abort (reifySk y) ⋆`: applying it enters `eAbort`.

### Which sources can affect safety (reach eAbort, eH1, or the H node)?

* **eBnil's tag** — alone it cannot: the value `bnil s` is only *inspected* by
  `ap`.  Through `apBnil` it can: `ap (bnil s) a ↦ rdflt s`, and `rdflt s` for an
  arrow `s` is a closure that aborts.  So the tag reaches safety only through
  an application of the empty branch list.
* A typed run never applies the empty branch list: `bnil` is typed only at
  `tBrs P NL` (S's `tBrs` clause is vacuous at `k = NL`), a list typed at `k`
  holds `NL - k` branches, and labels are `< NL`.  `Ok` therefore *cannot
  have* an `apBnil` node in the existential proof (`apBnil` is used by no
  `s52*` file).
* **EvE's erasure choice** `e2`: it cannot be ignored, since it changes the
  code that runs.  It is covered by the budget induction (the decoded program
  runs at `m < n`), exactly as in the existential proof.
* The other disjunctions above are decided by data the premises already fix,
  so they are not choices.

Conclusion of the audit: the ONLY ways two runs of the same source can differ
are (i) the tag of `eBnil` (and its consequence `apBnil`), and (ii) in `EvE`,
the erasure `e2` of a reflected program.

## 2. How `Ok` relates to `Ev`

`Ok` is a *separate inductive family*, with the same indices `(n, src, v)`,
whose constructors are the constructors of `Ev` minus eAbort and eH1, with the
reflect rules additionally requiring `isH D = false`, and nested reflect runs
traced by `Ok` at the smaller budget (`OkE`: by `OkE` through the erasure).
`ok52_eval` / `ok52_evalE` prove `Ok ⊆ Ev`, `OkE ⊆ EvE` by induction.  So `Ok`
is not a predicate on `Ev` derivations; a universal statement quantifies over
`Ev` derivations and concludes with an `Ok` derivation *of the same index*
(same `n`, `src`, `v`).  That is what `theorem52_all` says.

## 3. Route

**Chosen: (b), with the free choices made harmless, not ignored.**  The existential
Theorem 5.2 stays as the reference run; a *determinism-up-to-tags* theorem for
`Ok` transfers its safety to every `Ev` run.

*Why not (a).*  Re-proving the S-relation argument for arbitrary runs means a
universal S (the arrow clause quantifying over runs), and with it new versions of
every structure lemma of S (`s52_subst`, `s52_lift`, `s52_cv`, the dependent
`recN`/`recS`/`itR`/`bcons` cases): the existing proof is ~3500 lines of
dependent denotation reasoning, and none of it carries over.

*Why (b) works.*  Two runs of one program differ only in (i) the tag of `eBnil`
(and `apBnil`, its consequence) and, for `EvE`, (ii) the erasure of a reflected
program (§1).  Define `flV v`, `v` with every bnil tag replaced by `Sk.unit`
(`s52sim.clj`; `RV` is nested, so this uses the nested recursor `RV.rec`/`RV.rec_1`).
Then the main lemma (`s52det.clj`, `detOk`) is:

> if `Ok n src v` is a safe trace, then for every `Ev n src' v'` with
> `src'` equal to `src` up to tags, `Ok n src' v'` and `flV v = flV v'`.

Proof: induction on the `Ok` trace (an explicit `Ok.rec`, one case lemma per rule),
inverting the `Ev` derivation at each node.  *Inversion* is by a function, not by
`cases` (unusable on the indexed family): `InvAll n src v` (`s52inv.clj`) is defined
by recursion on the source and states what a derivation of `(n, src, v)` consists of;
`ev_inv` proves, by one induction, that every derivation satisfies it.  The clauses
are generated from the constructor tables read from `eval.clj`/`erase.clj`.
The case lemmas are generated from the same tables, with a short list of extra
tactics for the rules whose `Ev` counterpart has several alternatives (ite, elimB,
inspect: refuted by the scrutinee; reflect: refuted by the guard and by `dec c`,
whose decoded triple is a function of `c`).  Nothing in the lemma mentions typing:
it is a statement about `Ok` and `Ev`.

*Why `Ok` must have no `apBnil`* (the one change to a definition, commit 37b7a6d):
with `apBnil` in `Ok`, the reference run could apply the empty list under one tag and
the arbitrary run under another, `rdflt` of two tags can differ in shape (an arrow
default is a closure that aborts), and determinism fails.  No `s52*` proof uses
`apBnil`: a typed run never applies the empty branch list (bnil is typed only at
`tBrs P NL`, `S`'s `tBrs` clause is vacuous at `k = NL`, labels are `< NL`).

*The `EvE` side and the second definition change.*  In `EvE` the reflect rule runs
ANY erasure `e2` of the decoded derivation.  The main lemma then needs the two
erasures' values to agree at the reflect node.  They do at a base type: `S` pins the
value to the denotation (`s52_base_unique`).  This needs (1) the budget induction
hypothesis to carry `S` at base types (`UE_E`, `BELOW_E`), (2) `D` base in the trace
rule.  Reflect is typed only at a base type, but `OkE` as it stood did not record it,
and an `OkE` derivation alone cannot supply it, so `OkE.eReflOk` gains the premise
`hB : isBaseTy D = true` (`erase.clj`; the existential proof supplies it from the
typing: `s52_refl_yes` already has `hbase`; `trace52_refl_ok` takes it).  `Ok.eReflOk`
(no erasure choice) and `Ev`, `EvE`, `S52` are unchanged.

*The theorems* (`s52univ.clj`).  `theorem52_all` = `theorem52` (a reference run) +
`detOk_tm`.  `theorem52e_all` = a strong induction on the budget (`ue_below`) whose
step (`ue_step`) is `s52_closed_all` (the reference run) + `detOkE_tm` under the
hypothesis for the smaller budgets; at a base type the value of the arbitrary run is
S-related because S is invariant under tags (`s52_base_sim`).
`theorem52_all_concrete`: both at `Check decCert` with `check_spec`.

## 4. Outcome (2026-10-07)

Proved (kernel-checked, `bin/formal-check` on the warm server; fresh suites
listed in the report):

* `theorem52_all`, `theorem52_all_base`: every `Ev` derivation of
  `Eval n (rtokens n) t v` from a derivable `Θₙ ⊢ t :¹ X` is `Ok n (tm (rtokens n) t) v`.
* `theorem52e_all`: for every erasure `e`, every `EvE` derivation of
  `EvalE n (rtokens n) e v` is `OkE`.
* `theorem52_all_concrete`: both at `Check decCert`, no hypotheses.

Not proved / not claimed: nothing from the task is open.  The statements are
`Ok`/`OkE` of the *same* `(n, src, v)` as the arbitrary run, so "reaches no
abort, H₁ or H node" includes nested reflect runs, and (by the removal of
`apBnil`) also "never applies the empty branch list".  No counterexample to
R4 §5 was found; the failure criterion did not trigger.  Relative to the
metatheory (Ansatz kernel), as for the whole formalization.

Definition changes, both listed above: `apBnil` removed from `Ok`/`OkE`;
`OkE.eReflOk` gains `hB`.  Neither is an axiom or a weakening of the theorem:
both make the safe trace stricter or record a fact the typing already gives.

