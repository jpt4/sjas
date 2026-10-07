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

(filled in below as the work proceeds)
