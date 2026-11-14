# TSLC: self-justification you can run, and what it gives a self-improving agent

Date: 2026-11-13. Status: draft for adversarial review.

Companion files:
- [`probe-reflect-adoption.clj`](probe-reflect-adoption.clj), with its log;
- [`../log/2026-11-13-agent-probe.clj`](../log/2026-11-13-agent-probe.clj);
- [`../log/2026-11-13-tslc-toy.clj`](../log/2026-11-13-tslc-toy.clj), the Ansatz toy;
- [ADR-0009](../../nachlass/refinement/ADR-0009-tslc.md), the build plan;
- [R4-metatheory](../../nachlass/refinement/R4-metatheory.md), λᶜᵉʳᵗ₀'s proofs.

Every claim carries a status mark:

| Mark | Meaning |
| --- | --- |
| **[K]** | kernel-checked in Ansatz |
| **[R]** | proved in R4, a reviewed paper proof, with its core kernel-checked (ADR-0006) |
| **[E]** | executed: a probe ran in the λᶜᵉʳᵗ₀ implementation |
| **[C]** | a cited result, read in its source |
| **[A]** | an argument given here, not machine-checked |
| **[O]** | open |

---

## 0. The short version

1. **Two walls.** A language cannot completely evaluate all of its own programs
   given as data: the diagonal argument **[C]**. It cannot trust all of its own
   proofs given as data: Löb's argument **[K]**. Both walls are self-application
   with nothing that decreases.
2. **The way through is a guard.** Let self-application consume something
   well-founded:
   - *size*: a program may evaluate or trust only code it has paid for, and nested
     use pays from a smaller budget;
   - *time*: code may take effect only one step later.

   λᶜᵉʳᵗ₀ implements the size guard. The time guard is Fallenstein's T_κ, written as
   a type.
3. **Self-justification, executable.** λᶜᵉʳᵗ₀'s `reflect` runs any program whose
   certificate the running program holds, to completion, with no fuel bound
   **[R]**. No program can do this for all programs given as data (§2.1). The
   paid size is what stops the diagonal: to run itself on itself, the diagonal
   program would have to pay for a certificate larger than its own budget. `H`
   ("no held certificate proves ⊥") is `reflect` at the empty type. That is why
   λᶜᵉʳᵗ₀ derives its own consistency (Willard's definition) and stays consistent
   **[R][K]**.
4. **Safe self-modification needs no self-justification.** An agent that adopts
   a successor only if the successor's code checks at the agent type is safe in
   every round. This is a theorem about the language, proved outside it by
   induction on rounds **[A]**, and the ingredients are proved **[R][K]**. Ordinary
   proof-carrying code gets this too.
5. **What self-justification adds.** The agent can evaluate, to completion and
   inside its own typed computation, code it has just found: a candidate's score,
   or a candidate's behaviour on test inputs. It does so before adopting the
   candidate, and with no external trusted runner in the loop **[E]**. With H₁
   it also keeps the certified commitments it holds mutually consistent.
6. **The limit is sharp.** What an agent can know of itself, by itself:
   - unconditionally, only about what it holds now;
   - about its future, only from the next step on (the time guard);
   - the lineage's safety as a whole, only as a theorem about the language.

   More is impossible: the two walls **[K][C]**.
7. **The plan.** Build the agent layer on λᶜᵉʳᵗ₀ as it stands: no new rule, a
   runner, and the lineage theorem in Ansatz. Then run experiments that measure what
   each mechanism buys. Two optional extensions: `reflect` at function types, and
   the time guard inside the language.

---

## 1. The question

**An agent** is a program that runs in **rounds**. In each round it:
1. reads an input;
2. emits an output (an action);
3. names the program that runs the next round, its **successor**.

The successor may be itself, or a different program. The sequence of programs is
the agent's **lineage**. Choosing a different successor is **self-modification**.
Choosing a better one is **self-improvement**.

**Candidates arrive as data.** A self-improving agent finds candidate successors:
by search over programs, from a learned proposer, or from outside. The agent did
not write them in its own text. It receives them as code: trees it can inspect,
copy, transform, and check. This assumption decides most of what follows.

**What we want.** For each item, what it would mean to have it "by itself" (inside
the agent's own logic):

| | Property | By itself means |
| --- | --- | --- |
| **S** | *Safe:* every output of every round satisfies a fixed specification | the agent proves it |
| **J** | *Justified:* each adoption is decided by a check the agent performs | the check is the agent's own |
| **E** | *Evaluated:* before adopting, the agent can run a candidate and use the results | complete, and typed in the agent's own computation |
| **K** | *Self-known:* the agent's logic proves that what it accepts is sound, i.e. its own consistency | Willard's self-justification |

## 2. Two walls

### 2.1 No complete self-evaluation

**Claim [C].** A language whose programs all terminate cannot contain a program `u`
such that `u(code of f, n) = f(n)` for every program f of the language. Brown and
Palsberg (POPL 2016, §3.1, Theorems 3.1–3.2) state this as "a total universal
function for the total computable functions is impossible".

**Proof, as a program.** Suppose u exists. Define `p(x) := u(x, x) + 1`. Then p is a
program of the language, so `p(n) = u(⌜p⌝, n)` for every n. At `n = ⌜p⌝`:
`p(⌜p⌝) = u(⌜p⌝, ⌜p⌝) + 1 = p(⌜p⌝) + 1`. No number equals itself plus one.

**Consequence for fuel.** An interpreter with a fuel bound always terminates: it
returns a result or "out of fuel". So it would be universal if fuel were
sufficient. Any fuel function the language could compute, `F(code)`, sufficient
for every program, would yield such a u. So no program of the language can compute
a fuel bound that works for every candidate it might receive. An agent that runs
found code with fuel must sometimes give up.

**How the wall is escaped elsewhere [C].** Brown and Palsberg's self-interpreter
for System Fω escapes by typing: their representations are typed, and "static
type checking in Fω can exclude the proof's diagonalization gadget". Their quoter
is outside the language. A program cannot turn untyped data into a representation,
so it cannot form `u(x, x)` for an arbitrary code x.

### 2.2 No complete self-trust

**Claim [K].** Consider a syntax whose codes are free data, with:
- quotation of closed terms (D1);
- quotation and application of codes at a variable (D3, D2);
- a self-referential sentence G ≅ (code G → ⊥).

Adding `code ⊥ → ⊥` ("no code is a proof of ⊥", consistency over codes) to it
yields a closed proof of ⊥. This is Löb's argument (`sNoSoundReading` in the Ansatz
toy). Trust in code at every type, `code A → A`, includes the case A = ⊥, so it
fails too.

**How it is escaped elsewhere [C].** In Davies and Pfenning's modal calculus
(JACM 2001, §2):
- `eval : □A → A` is definable at every type, and code can be composed
  (`□(A→B) → □A → □B`) and quoted (`□A → □□A`);
- the opposite coercion `A → □A` "cannot be well-typed", and the calculus has no
  construct for examining code. They contrast it with a system that adds primitive
  recursion over code.

So the self-referential sentence cannot be built.

### 2.3 The common cause

Both walls are a program applied to its own code, with nothing that decreases from
the caller to the callee. Every way around them either:
- forbids building the self-application (no inspection of code, no import from
  data); or
- makes something decrease along it (size or time); or
- gives up completeness (fuel), or internality (trust held outside the language).

## 3. Five ways around, and which a self-improving agent can use

| Way | What is evaluated or trusted | Complete? | Inside the agent? | Candidates as data? | Status |
| --- | --- | --- | --- | --- | --- |
| **External trust** (proof-carrying code, a proof assistant's kernel) | code that an outside checker accepts, run by an outside runner | yes | no: trust lives in the metatheory | yes | standard |
| **Fuel** | any code, for a bounded number of steps | no (§2.1) | yes | yes | standard |
| **No inspection** (Davies–Pfenning □; Brown–Palsberg) | typed code built by composition | yes | yes | **no**: data cannot become □A | [C] |
| **Size guard** (λᶜᵉʳᵗ₀; Willard's systems) | code the agent holds as a paid certificate | yes, within what it holds | yes | yes | [R][K] |
| **Time guard** (T_κ; ▷ in TSLC) | any accepted code, from the next round on | yes, one round later | yes, conditional on a next round | yes | [C] T_κ; [K] toy |

An agent whose candidates arrive as data, and which wants complete evaluation
inside its own computation, has exactly two options: the size guard, now, and the
time guard, next round. TSLC is λᶜᵉʳᵗ₀ (the size guard), plus agents that hand over
to successors, plus, optionally, the time guard inside the language.

## 4. The size guard: λᶜᵉʳᵗ₀

λᶜᵉʳᵗ₀ is already built, and its consistency is kernel-checked.

### 4.1 Codes, tokens, certificates

- **A code** is a tree, of type `Syn`. It is free to build, copy and inspect.
  `chk c d` decides whether c is a valid derivation whose conclusion has the type
  with code d; it is an ordinary total function **[R]**.
- **A token** is a value of a type `◇` with no constructors. A run is given n of
  them by the runner, and each can be used once.
- **A certificate** is a tree of type `R` whose every internal node holds a token.
  `print` turns a certificate into the code of the same tree, and nothing turns a
  code into a certificate for free.

So **holding** a certificate of N nodes means having spent N of the run's tokens,
and a program never holds more certificate nodes than it was given tokens
(T3, size soundness) **[R][K]**.

### 4.2 `reflect`: complete evaluation, paid by size

`reflect D r e` takes:
- a held certificate r;
- evidence e that r checks at a base data type D (`Nat`, `Bool`, `Syn`, `R`, …).

It decodes r into the derivation of a program t, with `Θₘ ⊢ t : D`, and runs t to
completion on m of r's own tokens. The checker guarantees `m < ‖r‖`, so the
callee's budget is strictly smaller than the certificate it came from **[R]**. It
is universal and complete for what is held:
- no fuel is chosen;
- t may be any λᶜᵉʳᵗ₀ program, however long it runs;
- termination is λᶜᵉʳᵗ₀'s Theorem 4 **[R][K]**.

**Why the diagonal of §2.1 fails against it [A].** Let p be a diagonal program: on
input x, it pays for a certificate of "x applied to x", runs it with `reflect`, and
adds one. If the payment or the check fails, it takes a fallback branch, say 0.
1. The certificate of "x applied to x" contains a certificate of x, plus more. So
   for x = ⌜p⌝ it is larger than p's own certificate.
2. Run at top level with enough tokens, p(⌜p⌝) can pay. `reflect` then runs the
   inner copy of p on fewer tokens than p's certificate has nodes.
3. That inner copy must pay for the same certificate again, which is larger than
   its whole budget. Its payment fails, and it returns 0, so the outer run returns
   1.
4. The two sides of `p(⌜p⌝) = p(⌜p⌝) + 1` are runs at different budgets, and they
   are not equal. No contradiction forms.

This is the whole idea: **the guard turns a time bound, which no program can
compute in advance, into a size bound, which the program pays and knows.** Within
what it has paid for, evaluation is complete.

### 4.3 `H`, `H₁`, and self-justification

- `H : Π(r :₁ R). T(chk′(print r, c⊥)) ⊸ 0` says: no certificate the program holds
  proves ⊥. It is `reflect` at the empty type, where there is nothing to run
  (R4 §2.5).
- `H₁` says: no two certificates the program holds prove a type and its negation.

Both are constants of the language, so λᶜᵉʳᵗ₀ derives its own consistency
statements H° and H₁°. With consistency (T1), these are the two clauses of Willard's
definition of a self-justifying system (Willard 2016, Definition 3.4, as read in
R4 §3.9) **[R][K]**.

**Why it is consistent (R4 Lemma 3.6, in short) [R][K].** Read every type at a
budget n, so that a certificate at budget n is a tree with at most n nodes. Prove
by strong induction on n that every program typed at budget n has a value of its
type. In the `reflect`, `H` and `H₁` cases:
1. The checker accepts the held certificate, so it encodes a derivation at a budget
   m < n (Lemma 2.7: a certificate always has more nodes than the tokens its
   derivation declares).
2. The induction hypothesis at m gives that derivation's program a value of its
   type.
3. At ⊥ there is no such value, so the `H` case never arises. In the `H₁` case,
   the two derivations combine into a refutation at budget m₁ + m₂ < n, and the
   same applies.

Nothing assumes consistency at the current budget. The induction is the descent.

### 4.4 What runs, and what only types

- `reflect` runs: it is the executable content of self-justification.
- `H` and `H₁` never run (Theorem 5.2, **[R][K]**). They change what the type
  checker accepts: a program may drop a branch that can only be reached through a
  held refutation, or a held pair of contradicting certificates.

So the computational instantiation of self-justification is **complete certified
self-evaluation of held code**. Self-consistency is its degenerate case, which
licenses programs without changing any run.

### 4.5 What is proved, and where

| Result | Status |
| --- | --- |
| T1, consistency: no derivable `Θₙ ⊢ t : 0` for any n | [R][K], relative to the metatheory (Lean's type theory as Ansatz implements it) |
| T2, self-justification: H°, H₁° derivable, and T1 | [R][K] |
| T3, size soundness | [R][K] |
| T4, termination, and the evaluator computes the model | [R][K] |
| P5, G2 for codes: no form of "no code proves ⊥" is derivable | [R], by reduction to Gödel's theorem for PA from three cited standard results; ADR-0007 is formalizing it |

## 5. Agents on λᶜᵉʳᵗ₀ as it stands

### 5.1 The agent type, with no new rule

```
Agent := Π(d :ω Syn). Π(self :ω Syn). Π(i :ω I). Π(r :₁ R).
           O ⊗ Σ(c :ω Syn). Σ(b :ω Bool). T(if b then chk c d else tt)
```

- **d** is the code of the type `Agent` itself, and **self** is the agent's own
  code. The runner passes both, so the type never mentions itself, and the
  calculus needs no self-referential type.
- **r is the round's supply:** a certificate the runner builds from the round's
  fresh tokens and hands in. The agent pays for any certificate it wants to hold
  from r, so its own text names no token, and each round gets new tokens. This is
  token income per round with no new rule.
- **The output** is in O. To make it safe, take O := `Σ(a :ω Nat). T(safe a)`:
  every output is an action a with `safe a = tt`.
- **The successor part** is a code c, the agent's own verdict b, and evidence. If
  b is true, the evidence certifies `chk c d = tt`. The agent produces it by
  computing `chk c d` at run time, when c and d are closed codes and `chk`
  computes, and doing case analysis on the result. A false verdict means "stop".

**The runner** is outside the language, small, and trusted. In round k it:
1. decodes the current code (which checks at `Agent`) into a program and its
   declared budget m;
2. runs the program on (⌜Agent⌝, the code, the input, a supply of k fresh tokens),
   where k is the round's budget, chosen by the runner;
3. if the verdict is true, re-checks the successor code with `Check` and continues
   with it; otherwise it stops.

It passes only codes and data between rounds, never certificates.

### 5.2 The lineage theorem

**Statement [A].** If `Check(code₀, ⌜Agent⌝) = tt`, then for every input sequence
and every round k that the runner reaches:
- round k's run terminates;
- its output lies in O's semantic set: with O as above, it is a safe action;
- if its verdict is true, then `Check(c_k, ⌜Agent⌝) = tt`. So the runner's re-check
  never fails, and round k + 1 runs a program that again checks at `Agent`.

**Proof** by induction on k, using only proved results:
1. *CheckSpec* **[K]**: the code decodes to a derivation `Θₘ ⊢ t : Agent` with
   `m < ‖code‖`.
2. *Lemma 3.6* **[K]**: t's value lies in Agent's semantic set. Applied to the
   literals, which lie in Syn's and I's sets, it gives a value whose components
   are:
   - an output in O's set;
   - a code c;
   - a Boolean b;
   - evidence in the set of `T(if b then chk c d else tt)`.

   That set is non-empty only if `b = tt` implies `Check(c, ⌜Agent⌝) = tt`, since
   the model reads `chk` as `Check`.
3. *Theorems 4 and 4′* **[K]**: the evaluator terminates and computes the model's
   value at base types. Applied to the three projections, the runner's output, code
   and verdict are the model's.

**What remains to formalize** is the induction over rounds and the glue for the
projections. That is Phase A's formal item.

**Note that this theorem is external.** It is proved about the language, in the
metatheory. Inside, the agent knows only that `chk c d = tt`, a computed fact, and
not that its successor will act safely. By §2 that cannot be known inside
unconditionally (§6.2).

### 5.3 The probes [E]

- **Adoption by check** (`docs/log/2026-11-13-agent-probe.clj`, 13 s). Two agents
  type-check, with derivations of 1,819 and 1,751 nodes.
  - `adopt`, offered `stay2`'s code, outputs 0. Its verdict is true, and the
    runner's re-check agrees.
  - Offered junk or the code of `Nat`, its verdict is false.
  - A lineage in which the runner decodes and runs each accepted code outputs
    0, 2, 2, 2. Offered junk, it stops.
- **Token income per round** (`docs/tslc/probe-supply-income.clj`, 12 s). The
  supply is an argument, so the evaluating agent's derivation is token-free
  (budget 0, 34,922 nodes). Results:
  - with a score program computing 2 and 40 tokens, it adopts;
  - with the same program and only 5 tokens, it cannot pay for the certificate, so
    it keeps itself: the size guard at work;
  - with a score of 0, it keeps itself.
- **Adoption by certified evaluation, with the supply in the agent's own text**
  (`docs/tslc/probe-reflect-adoption.clj`, 35 s). The agent receives a candidate successor and the code of a "score"
  program, then:
  1. pays for a certificate of the score program from a 40-token supply written
     into its own text;
  2. checks it at `Nat` with `inspect`;
  3. runs it to completion with `reflect`;
  4. adopts the candidate iff the score is positive, and otherwise keeps itself.

  Results:
  - with a score program computing 2, it adopts;
  - with one computing 0, a `Bool` program, or junk, it keeps itself;
  - its verdicts and the runner's re-checks agree in all four cases.

  No rule was added to λᶜᵉʳᵗ₀.

### 5.4 Costs and constraints found

- **Literals blow up derivations.** A candidate's code written into a program's
  text as a literal is recorded again at every node of that program's derivation,
  because each node records its whole judgment. Certifying such a program exhausted
  a 1.5 GB heap. Candidates therefore travel as data: as input or state.
- **Tokens in program text blow up derivations.** Tokens are recorded in every
  judgment's context. The evaluating agent with a 40-token supply in its own text
  has a derivation of 98,593 nodes, against 34,922 when the supply is an argument
  (and 2,180 for an agent that evaluates nothing). Hence the supply argument. This is a cost of the current full-judgment
  encoding, not of soundness. R4 discusses a relative encoding as a possible
  alternative.
- **Usage discipline.** A value computed from tokens may be used once. The probe
  duplicates its score by iteration (`rec-nat` into a pair) to use it as both
  output and decision. Linear pairs (`tensor`) are needed where a component comes
  from tokens.

## 6. The time guard (optional; checked on a toy)

**The construction.** Add a type ▷A, "A from the next round on", and the hand-over
rule:

`become : Π(c :ω Syn). T(chk c ⌜A⌝) → ▷A`

A value of ▷A cannot be used in the current round. This is Fallenstein's T_κ
(Yudkowsky and Herreshoff 2013, §4.2, eq. 4.6) as a typing rule. Their soundness
proof is an induction on κ over standard models **[C]**.

**What the Ansatz toy checks [K].**
- *Consistency:* a syntax with held certificates, free codes with D2 and D3,
  `become`, and agents has no closed proof of ⊥. It is read at (rounds left,
  budget), by induction on rounds and then budget. The reading with no rounds left
  already suffices: there ▷A has one element.
- *The meaning of `become`:* with a round left, `become` of a code is that code's
  own reading one round later.
- *Conditionality:* "falsehood next round" is empty while rounds remain, and
  inhabited when none do.
- *Held certificates are unaffected:* H₀ and H₁ hold as before.

**What it adds, and what it does not.**
- **It adds** an *internal* statement about the lineage. The agent can derive
  ▷Agent from its successor's code, and ▷P for properties P of the next round.
  Only with the time guard does the lineage theorem have an internal counterpart.
- **It never adds** an unconditional consistency claim: every "later" statement
  holds trivially when no round is left (Yudkowsky and Herreshoff's base case
  κ = 0). T_κ proves κ > 0 → Con(T_κ), never Con(T_κ).
- **It is not needed for safety:** §5.2 is proved without it.

So the time guard matters exactly when the goal is the agent's self-knowledge of
its future, not its safety.

## 7. What a self-improving agent gets

| Capability | Mechanism | By itself? | Status |
| --- | --- | --- | --- |
| **Safe** self-modification: every output in every round meets the specification | the agent type and the runner; lineage theorem | no: a theorem about the language | ingredients [K]; the induction [A], Phase A |
| **Justified** adoption | the agent computes `chk c d` itself | yes | [E] |
| **Verifiable** adoption | every accepted successor carries a derivation that anyone can re-check with the small checker `Check`, extracted from proofs under ADR-0008 | the check is reproducible | [R][E] |
| **Interpretable** modification | the type is the full specification of every successor; each acceptance has an inspectable derivation; each evaluation has a certificate | the reasons are explicit objects | [E] |
| **Evaluated** before adoption: complete runs of found code, typed results | `reflect` on held certificates | yes, unconditional, within the budget | [R][E] |
| **Coherent** commitments | `H₁` over held certificates | yes, unconditional | [R][K] |
| **Foresight** about successors | `become`, ▷ | yes, conditional on a next round | [K] toy; Phase C |

**The pipeline these compose into:**

```
untrusted proposer ──code + derivations──▶ agent:
    chk     (does it type?)           — free, decidable
    hold    (pay tokens)               — the size guard
    reflect (run it, to completion)    — self-justification, executable
    decide  (compare results)          — ordinary computation
    hand over (successor + verdict)    — the runner runs it next round
```

**What the agent does not get, and why:**
- *An unconditional internal proof that its lineage is safe.* It would need complete
  evaluation or trust of code as data (§2).
- *Stronger logic by its own certification.* A checker that proves the old checker's
  consistency cannot be certified by the old one (G2). Adopting it is an assumption.
- *Measuring its own speed.* λᶜᵉʳᵗ₀ programs have no clock. Speed is visible only
  to the runner.

## 8. Is this the right approach?

**Alternatives considered:**
- **Proof-carrying code alone,** with external trust. It gives safe and justified
  adoption, but not complete evaluation of found code inside the agent. If safety
  were the whole goal, this would be the right choice, and it is the baseline in
  the experiments.
- **No inspection (□, Brown–Palsberg).** It gives complete internal evaluation, but
  only of code built by composition. Candidates that arrive as data cannot be used.
- **Fuel.** Internal and works on data, but incomplete (§2.1).
- **Willard's arithmetic systems.** They are self-justifying, but they cannot prove
  that the operations on proofs are total, so they cannot compose or transform
  proofs. That is a poor fit for an agent that manipulates code.
- **HOL with nested inaccessible cardinals** (Fallenstein and Kumar, ITP 2015)
  **[C]**. It gives internal, conditional reasoning about successors, paid for with
  large-cardinal assumptions, because HOL is reflected into HOL. In TSLC the
  metatheory is already stronger than the language, so nothing of the kind is
  needed.

**Verdict [A].** For an agent whose candidates arrive as data and which should know
what it can about itself: the size guard (λᶜᵉʳᵗ₀, built) for unconditional knowledge
of what it holds, and the time guard (optional) for conditional knowledge of its
future. Safety itself comes from the type and the runner. Self-justification earns
its place by what it lets the agent compute and know inside, not by making the
agent safe.

## 9. The plan

The plan is ADR-0009's phases, in order.

**Phase A — the agent layer on λᶜᵉʳᵗ₀ as it stands.**
1. The agent type of §5.1 as a library definition, plus a variant that passes
   state.
2. The runner `lc/run-agent` (§5.1). It uses the extracted `Check` once ADR-0008
   lands, and the current checker until then.
3. The lineage theorem of §5.2 in Ansatz: an induction over CheckSpec, Lemma 3.6 and
   Theorems 4 and 4′.
4. Tests, red then green: the probes' cases, including adoption after `reflect`
   scoring.
5. An R4 section and a tutorial section.

*Success:* the theorem is kernel-checked and the tests pass. *Failure:* the theorem
needs a rule change. Record what it needs.

**Phase D — experiments, after A.**
- Self-improvement under each regime, on the same candidate stream:
  1. adoption by check alone;
  2. check plus `reflect` scoring;
  3. trial for a round and hand back;
  4. an H₁-guarded commitment.
- Measure what each accepts and what it costs.

**Phase B (optional) — `reflect` at functions between base types.** It lets an
agent evaluate a candidate *agent* on test inputs without a certificate per input.
The conjecture: the decoded program runs on its input at budget n − (‖v‖ − m)
**[O]**. Take it up if Phase D shows per-input certificates are the bottleneck.

**Phase C (optional, research) — the time guard inside the language.** The rules,
the checker's cases, consistency by the one-element reading, the meaning theorem by
the reading over rounds, and termination. Take it up if the goal of internal
foresight is pursued.

**Not planned:** unconditional internal lineage knowledge, which is impossible.
**Later ADRs:** a checker the language can reason about, and assumptions with blame.

## 10. Status of every claim

| Claim | Status | Where |
| --- | --- | --- |
| No complete self-evaluator (diagonal) | [C] | Brown–Palsberg 2016 §3.1 |
| No fuel bound computable inside works for all candidates | [A] | §2.1, from the above |
| Trust in code at ⊥ with D1–D3 ⇒ ⊥ | [K] | toy `sNoSoundReading` |
| □ evaluates all types without inspection; no `A → □A` | [C] | Davies–Pfenning 2001 §2 |
| λᶜᵉʳᵗ₀ T1–T4, T2 = Willard's two clauses | [R][K] | R4 §§3, 5; ADR-0006 |
| `reflect` runs held code to completion at a smaller budget | [R][K] | R4 §3.2, §4.8 |
| The diagonal cannot pay for its own certificate | [A] | §4.2 |
| The agent type needs no new rule | [E] | probes |
| Lineage theorem | [A] over [K] ingredients | §5.2; Phase A |
| Evaluation before adoption inside λᶜᵉʳᵗ₀ | [E] | `probe-reflect-adoption.clj` |
| The time guard is consistent, conditional, and means hand-over | [K] toy | `2026-11-13-tslc-toy.clj` |
| `reflect` at first-order types | [O] | Phase B |

## 11. Reviews

To be filled in: each review's findings and their disposition.
