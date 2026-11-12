# The trust horizon: EA-stability for computational self-justification

Date: 2026-11-05

Checks:
- Lean: [`2026-11-05-trust-horizon.lean`](2026-11-05-trust-horizon.lean), which
  extends [`2026-10-29-guarded-trust.lean`](2026-10-29-guarded-trust.lean) without
  changing it.
- Ansatz: [`2026-11-05-trust-horizon.clj`](2026-11-05-trust-horizon.clj), the same
  development in the environment λᶜᵉʳᵗ₀ is formalized in.

## Requests (verbatim)

> Willard remarks at times that E/A-stability are interesting properties unto
> themselves; why?

> What would the analogue of EA-stability be for computational systems that
> possess an analogue of self-justification?

> Yes, please try. Then, explain the computational analogue again.

> What is a contract? What does it mean by a certificate "reaching" a
> counterexample?

> Can you check it in Ansatz as well, since we are trying to use the Clojure
> environment for lambda-cert?

## Willard's property

Willard 2011 (arXiv:1108.6330v8), Definitions 4.1–4.4, 5.1, 5.3, 5.5, Remarks
5.2, 5.4, Appendix F. E(n) is 2ⁿ, and ♯(θ) is the largest J with every sentence
of θ true when its unbounded quantifiers are bounded by 2ᴶ. Both invariants range
over every r.e. set θ of Π₁ sentences, true or false. For a proof p of θ ∪ B with
Log(p) ≤ ♯(θ) + 1:

- A-stable: a Π₁ theorem is Good{½♯(θ)}.
- E-stable: a Σ₁ theorem is Good{½⌊Log(p)⌋ − 1}, a bound from the proof alone.

Willard's two reasons: short proofs give "partially useful" deductions
"irregardless of whether or not θ's axioms are technically true" (Remark 5.2),
which is how a Thinking Being can "partially tolerate its own errors"
(Appendix F); and "those same invariants" make Theorem 5.9's systems viable.

## The analogue

A certified program may use a contract it does not check. The contract has a
horizon J, the size of its smallest counterexample. Holding a value costs its
size, so a certificate reaches only values smaller than itself. Then, whether or
not the contract is true:

| Willard | Here | Lean |
| --- | --- | --- |
| (underlies both) | a refutation runs to a real counterexample smaller than itself | `blame` |
| Tight, Def 4.5 | no refutation fits under the horizon | `tightness`, `horizon_sharp` |
| E-stable | a certificate of ‘□’ A under the horizon returns an output smaller than itself | `output_below` |
| A-stable | a certified "no certificate of A" has no counterexample under the horizon | `universal_below` |
| self-justification | the contract is consistency, and blame is a smaller refutation | `consistent_when_contract_is_consistency` |

`blame` needs no hypothesis on the contract. It reads ‘⊥’ as "a counterexample
smaller than the level" (Friedman's A-translation), so a refutation is a program
that finds the bug. The horizon theorems are its contrapositives.

The contract on `boxes k` is true below k + 1 (`contract_true_below`), false at
k + 1 (`contract_false_at`), and refuted at size k + 4 (`refute`), so the bound
is sharp to within 3. `blame_refute` shows blame names the actual counterexample.
`fake` uses `abort` to certify a false claim above the horizon, and
`fake_is_blamed` shows the monitor catches it.

The worked example is checked in Ansatz as `hExample3`: the contract on `boxes 3`
first fails at size 4, and its refutation has size 7.

## The Ansatz check

Run from `code/lcert`, with `ANSATZ_OFFLINE=1` and the usual caps:
`clojure -M:formal -i ../../docs/log/2026-11-05-trust-horizon.clj`. A rejected
declaration aborts the load. On 2026-11-05, with Ansatz 0.2.115's bundled Init and
the default thread stack, it proved 36 theorems, refused all 5 negative checks,
and took about 30 seconds at about 700 MB.

| Lean | Ansatz |
| --- | --- |
| `blame` | `hblame`, `hBlameSmaller` |
| `tightness` | `hTightness` |
| `output_below`, `no_fake_below` | `hOutputBelow`, `hNoFakeBelow` |
| `universal_below` | `hUniversalBelow` |
| `consistent_when_contract_is_consistency` | `hConsistentWhenContractIsConsistency` |
| `horizon_sharp`, `blame_refute`, `fake_is_blamed` | `hHorizonSharp`, `hBlameRefute`, `hFakeIsBlamed` |
| `data_contract_refuted` | `hDataContractRefuted` |

Differences:

- **Axioms.** Ansatz has no `#print axioms`, so the file walks kernel terms itself,
  with a positive control (`hAuditUse`). `hblame`, `hTightness`, `hOutputBelow`,
  `hUniversalBelow`, the consistency theorem, `hBlameRefute` and `hFakeIsBlamed`
  reach no axiom. `hHorizonSharp` reaches `propext` and `Quot.sound`, through
  `omega` on goals of `False`. In Lean every theorem reaches those two.
- **`omega` and choice.** Ansatz's `omega` reaches `Classical.choice`, through
  `Classical.propDecidable`, when its goal is not `False`. The first complete port
  had choice in every core theorem for that reason alone, and explicit `Nat` lemmas
  removed it. λᶜᵉʳᵗ₀'s development has not been audited for this.
- **Recursion.** Lean's `termination_by` on the level becomes `Nat.rec` over a bound
  (`hBelowAll`), so a run reduces definitionally and `hBlameRefute` is `rfl`.
- **Surface.** Definitions are `kdef`s over explicit recursors with implicit
  arguments written out, and `HSum` is declared because Ansatz's Init has no `Sum`.
- **Negative checks.** Lean's were run on edited copies of the whole file. Ansatz's
  are five statements the file requires to be refused, printing each reason: the
  guard step with free quotation, internal quotation under the level, tightness with
  no size bound, the refutation under the horizon, and blame naming another term.

## Corrections to the first statement

The analogue was first proposed in chat before this check.

- *Program and input share the budget, playing the role of Willard's ½.* Wrong
  here: `universal_below` needs size f ≤ J and size x < J separately. The level
  is one cap. λᶜᵉʳᵗ₀'s linear tokens might still force a split, and where
  Willard's ½ comes from remains open.
- *The horizon is the core.* The core is `blame`, which is unconditional. The
  first statement did not have it.
- *Erasing the checks is justified by a theorem.* Not formalized. The file shows
  the monitor cannot fire under the horizon, which is the reason to expect it.

## What is load-bearing

The negative checks in the Lean header: free quotation breaks the `quo` clause,
an unbounded ‘□’ breaks `con` and `ax`, and internal quotation (D3) breaks by the
syntax it adds. So charging for what a certificate holds is what the theorem
needs. `data_contract_refuted` is the contrast, in plain Lean: a false contract
on `Nat` falls to a constant-size term, wherever its first violation lies.

## Not tested

- Contracts other than "no certificate of C". An arbitrary decidable predicate
  needs a richer type language than this toy has.
- λᶜᵉʳᵗ₀ itself. The guard here is syntax size, not tokens, and there is no data
  sort, recursion or linearity. That λᶜᵉʳᵗ₀ is stable on held certificates and
  not on `Nat` remains my expectation from T1 and T3.
- Witnesses checkable without θ. An output here may mention `ax` as data.
- The machine readings: blast radius of a checker bug, testing up to J as a
  certificate of horizon J, and retraction that keeps results under the horizon.
  They follow in the toy's terms but have not been built or measured.
