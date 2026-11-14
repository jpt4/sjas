# TSLC: what self-justification is, what it does in a running system, and what it does for a self-improving agent

Date: 2026-11-14. Revision 4, after four rounds of adversarial review (§11).

Companion files:
- probes [`probe-supply-income.clj`](probe-supply-income.clj),
  [`probe-reflect-adoption.clj`](probe-reflect-adoption.clj) and
  [`../log/2026-11-13-agent-probe.clj`](../log/2026-11-13-agent-probe.clj), with their logs;
- the Ansatz toy [`../log/2026-11-13-tslc-toy.clj`](../log/2026-11-13-tslc-toy.clj);
- the plan, [ADR-0009](../../nachlass/refinement/ADR-0009-tslc.md);
- λᶜᵉʳᵗ₀'s design [R4-certificate-calculus](../../nachlass/refinement/R4-certificate-calculus.md) and
  metatheory [R4-metatheory](../../nachlass/refinement/R4-metatheory.md);
- the reviews, in [`reviews/`](reviews/).

Every claim carries a status mark:

| Mark | Meaning |
| --- | --- |
| **[K]** | kernel-checked in Ansatz |
| **[R]** | proved in R4, a reviewed paper proof (its core [K] under ADR-0006) |
| **[E]** | executed: a probe ran in the λᶜᵉʳᵗ₀ implementation |
| **[C]** | a cited result, read in its source |
| **[A]** | an argument given here, not machine-checked |
| **[O]** | open |

---

## 0. The answer

1. **Safe self-modification does not come from self-justification.** An agent
   that adopts a successor only after checking the successor's code against a
   fixed agent type is safe in every round. This is ordinary proof-carrying code.
   It runs on λᶜᵉʳᵗ₀ as it stands, with no new rule **[E]**. Its safety is a theorem
   about the language, proved outside the agent by induction on rounds. The
   theorem's ingredients are proved **[R][K]**. The induction and two lemmas are still
   to be done **[A][O]**: P1 on projections, and P2 on runner correctness (§5.2).
2. **Self-justification in λᶜᵉʳᵗ₀ is two postulates and a proof that they are
   safe.**
   - `H` says no certificate the program holds proves ⊥.
   - `H₁` says no two held certificates prove a type and its negation.
   - The language derives these statements of its own consistency because they are
     constants of the language, in certificate form only. "No *code* proves ⊥" is
     not derivable (P5, Gödel's second theorem for codes) **[R]**.
   - The language stays consistent because every use of them descends to a strictly
     smaller budget **[R][K]**.
   - They never run (Theorem 5.2) **[R][K]**.
3. **The executable principle is `reflect`, bounded local reflection.**
   - `reflect` takes a held certificate of a base-data-type program, such as a
     `Nat` program, and runs that program to completion inside the current run
     **[R][K]**.
   - `H` is its instance at ⊥, where there is nothing to run.
   - What `reflect` buys: a fixed program can extend its own computation with code
     it receives at run time, and the computation is guaranteed to finish.
   - The cost is the certificate's size, in tokens, computable from the code before
     running it. Tokens bound the certificate, **not the running time**: the
     language's ordinary layer has the strength of Gödel's System T, so a small
     certificate can run for an astronomically long time **[R]**.
   - **The competitor it must beat** is a *step-limited evaluator* written in
     λᶜᵉʳᵗ₀ itself: a program `run(c, x, k)` that runs code c on input x for at most
     k steps and returns either the result or "out of steps". The ordinary layer
     computes on codes with System T's strength, so it can define one (Kleene's
     step-counting predicate is primitive recursive) **[A]**; it is not built yet.
     - It needs no certificate, spends no tokens, works on any input the agent
       picks during the run, and bounds time.
     - What it lacks is completeness: it can answer "out of steps". No step bound
       computable inside the language suffices for every program (§2.1) **[C][A]**.
   - So `reflect`'s advantages over it are **completeness** and lesser ones: a result
     of type D rather than "D or out of steps"; reuse of the evaluator R4 already
     proves correct, where the step-limited one needs its own proof; native speed,
     unless the implementation adds a native step-limited primitive (which is safe,
     because such a function is already definable); and a logged certificate per
     evaluation. Its disadvantages are a certificate per (candidate, input) and no
     time bound **[A]** (round-4 review).
4. **For a self-improving agent, self-justification is not needed for any
   capability shown so far.**
   - *Safety and adoption* come from typed hand-over (point 1). Trial for a round,
     and hand-over to a checked wrapper that runs the candidate and reverts, are
     also safe by the type alone.
   - *Evaluating found code inside the run* comes from the step-limited evaluator,
     which needs no self-justification.

   Whether `reflect`'s completeness gives a measurable gain over all three is a
   **hypothesis**. The plan tests it with a criterion that can refute it (§9), and
   the predicted outcome is that it does not.
5. **What self-justification is for: knowing where self-trust stops.**
   - λᶜᵉʳᵗ₀ shows that a running system can state its own consistency (in
     certificate form) and completely evaluate its own certified extensions,
     without contradiction **[R][K]**.
   - It also shows the boundary: not for codes (P5), not at higher types without new
     proof (Lemma 3.4), not unguarded (§2) **[R][K][C]**.
   - That is a result about the limits of self-knowledge, which is the research aim.
     It is not a tool that makes agents safer or better.
   - The capability an agent would want from self-trust is different: using a
     *proof of a universal statement* received at run time, such as "this successor
     is safe on every input", in its own reasoning. That needs reflection at
     function types, which is open **[O]**. Even its practical value is
     unestablished, because typed hand-over already routes every such guarantee
     through `chk` and the external theorem.
6. **The limits** apply among the mechanisms known here. No theorem says the list is
   exhaustive **[A]**.
   - Unconditional internal knowledge reaches only what the agent holds.
   - Knowledge of its future is available only conditionally, through a time guard,
     and only at the toy level so far.
   - The lineage's safety is available only as an external theorem.
   - Unguarded trust in code is inconsistent: Löb's argument in the toy **[K]**,
     and P5 for λᶜᵉʳᵗ₀ **[R]**. Unguarded complete evaluation of code is impossible
     **[C]**.
7. **The plan.**
   - **Phase A** builds the agent layer: the agent type, a runner, the lineage
     theorem in Ansatz, and a gating probe that checks whether an agent can
     evaluate a *candidate's own behaviour* before adopting it. The earlier draft
     wrongly claimed this was already shown (§11). It has since passed at toy size
     **[E]** (§5.3).
   - **Phase D** runs a falsifiable experiment: does in-run evaluation with
     `reflect` beat trial, wrapper hand-over and the step-limited evaluator?
   - **Phases B and C** (reflection beyond base types, the time guard in the
     language) are research on the limits of self-trust, not agent engineering.

---

## 1. The question

**Terms.**
- **An agent** runs in **rounds**. In each round it reads an input, emits an
  output, and names the program for the next round, its **successor**.
- **The lineage** is the sequence of programs. **Self-modification** is naming a
  different successor; **self-improvement** is naming a better one.
- **A runner** is the program outside the language that starts each round.

**Candidates arrive as data.** Candidate successors come from search, from a
learned proposer, or from outside. The agent receives them as **code**: trees it can
inspect, copy and check, not terms written in its own text.

**What one might want.** For each item, what it would mean to have it "by itself":

| | Property | By itself means |
| --- | --- | --- |
| **S** | *Safe:* every output of every round meets a fixed specification | the agent proves it |
| **J** | *Justified:* each adoption follows a check | the agent performs the check |
| **E** | *Evaluated:* before adopting, the agent runs a candidate and uses the results | the run is inside the agent's own computation and completes |
| **K** | *Self-consistent:* the agent's logic states its own consistency | Willard's self-justification |

E is a form of *reflection*: trusting what a checked proof computes. K is
reflection at ⊥.

## 2. Two walls

### 2.1 No complete self-evaluation in a total language

**Claim [C].** Brown and Palsberg (POPL 2016, §3.1, Theorems 3.1–3.2) state that "a
total universal function for the total computable functions is impossible". In a
language whose programs all terminate (λᶜᵉʳᵗ₀ is one, by T4), no program
`u : Syn → Syn → Nat` can satisfy `u(⌜f⌝, x) = f(x)` for every program `f : Syn → Nat`
of the language.

**Proof.** Let `p(x) := u(x, x) + 1`. Then p is a program, so
`p(x) = u(⌜p⌝, x)` for every x. At `x = ⌜p⌝`, `p(⌜p⌝) = p(⌜p⌝) + 1`, which no number
satisfies. Here ⌜p⌝ is p's code, written outside the language. p needs only to pass
its argument to u twice.

**Fuel [A].** Suppose the language contained:
- an interpreter `run(c, x, k)` that runs program c on input x for k steps, covering
  everything the language has, including `chk`, `inspect`, `reflect` and tokens;
- a fuel function `F(c, x)`, computed inside the language, enough for every program.

Then `run(c, x, F(c, x))` would be such a u. So no fuel bound computed inside works
for every candidate. A runner outside can supply fuel, but nothing tells the agent
in advance how much a candidate needs.

### 2.2 No complete trust in code

**In the toy [K].** The toy's syntax has:
- codes as free data, with D1, and D2 and D3 at a variable;
- a built-in self-referential type G ≅ (code G → ⊥).

Adding `code ⊥ → ⊥` to it yields a closed proof of ⊥, `sViaQuine`; hence no reading
sends ⊥ to the empty set, `sNoSoundReading`. Trust in code at every type,
`code A → A`, includes A = ⊥.

**For λᶜᵉʳᵗ₀ itself [R].** P5: no form of "no code proves ⊥" is derivable at any
budget. The proof is a reduction to Gödel's theorem for PA, from three cited
standard results; ADR-0007 is formalizing it.

### 2.3 The common cause, and how it is escaped

Both walls are a program applied to its own code, with nothing that decreases from
caller to callee. The escapes known here:
- **External trust.** The trust lives outside the language, in its metatheory.
- **Well-founded descent.** Self-application consumes something that decreases:
  *size* (paid certificates), *time* (one round later), or *strength* (an
  interpreter for a weaker language).
- **No inspection.** The self-application cannot be built at all.
- **Fuel.** Gives up completeness.

## 3. The escapes, side by side

| Escape | What is run or trusted | Complete? | Inside? | Candidates as data? | Cost known in advance? | Status |
| --- | --- | --- | --- | --- | --- | --- |
| **External trust** (proof-carrying code; a proof assistant's kernel) | code an outside checker accepts, run by an outside runner | yes | no | yes | n/a | standard |
| **Fuel** (e.g. a step-limited evaluator written in λᶜᵉʳᵗ₀) | any code, for k steps | no (§2.1) | yes | yes | fuel needed: no; time: bounded by k | standard; [A] for λᶜᵉʳᵗ₀ |
| **No inspection** (Davies–Pfenning's `eval : □A → A`; Brown–Palsberg's Fω self-interpreter) | typed code built by composition | yes | yes | no: data never becomes □A ("λx:A. box x cannot be well-typed", DP §2) | n/a | [C] |
| **Strength** (an interpreter for a weaker language) | code of the weaker language | yes, for that language | yes | yes, if in the weaker language | yes | standard; λᶜᵉʳᵗ₀'s budgets form such a tower |
| **Size** (λᶜᵉʳᵗ₀ `reflect`; in arithmetic form, Willard's systems) | held certificates of base-type programs | yes, if the tokens are there | yes | yes: `chk` checks data | tokens: yes, the certificate's node count; time: no | [R][K] |
| **Time** (T_κ; ▷ and `become`) | any accepted code, as a program that runs next round | its result arrives next round, not now | trust inside, conditional on a next round | design: yes, via `chk`; toy: no, its codes are typed terms | n/a | [C] T_κ; [K] toy |

**What size buys over fuel** is completeness, not speed or a time bound. The tokens
a candidate needs are its certificate's node count, which the agent can compute
before deciding, and once they are paid, completion is guaranteed (T4). But tokens
bound the certificate's size, not the running time, which can be astronomically
long. Fuel bounds time and may give no answer; the fuel a candidate needs cannot be
computed in advance by any program of the language (§2.1). Neither dominates.
Earlier revisions called the token cost "predictable" as if that were an operational
advantage; round 4 corrected this (§0.3, §11).

## 4. λᶜᵉʳᵗ₀: the size guard, already built

### 4.1 Codes, tokens, certificates

- **A code** (`Syn`) is a tree, free to build, copy and inspect. `chk c d` decides
  whether c is a valid derivation whose conclusion has the type with code d; it is a
  total primitive recursive function **[R]**.
- **A token** is a value of a type with no constructors. A run is typed in a context
  of n tokens, Θₙ, and uses each at most once.
- **A certificate** (`R`) is a tree whose every internal node holds a token. `print`
  turns it into the code of the same tree; nothing turns a code into a certificate
  for free.

**Holding.** A program holds a certificate of N nodes only after spending N tokens.
At run time this is T3 together with Theorem 4′ for the erasing evaluator
**[R][K]**. A program can spend only tokens its text names, or a certificate
supply passed to it (R4 §1.1a).

### 4.2 `reflect`: bounded local reflection at base types

`reflect D r e` takes:
- a held certificate r;
- evidence e that r checks at a **base data type** D (`Nat`, `Bool`, `Syn`, `R`, …).

It decodes `Θₘ ⊢ t : D` from r, with m < ‖r‖ (Lemma 2.7), and runs t to completion on
m of r's own tokens **[R][K]**. If r has more nodes than the current budget, it
returns a default value. For higher types the soundness of `reflect` is open, since
Lemma 3.4 fails there **[O]**.

**Why self-application causes no paradox [R].** The model reads `reflect` at budget n
by recursion to a budget m < ‖r‖ ≤ n (R4 §3.2), so no equation between two runs
ever holds at a single budget. Take a diagonal program that, given its own code and a
supply, pays for a certificate of "itself applied to itself, with a smaller supply"
and reflects it. It simply recurses as deep as its supply allows, a recursion that is
well-founded on the supply's size. The two sides of `p(⌜p⌝) = p(⌜p⌝) + 1` are runs at
different budgets.

### 4.3 `H`, `H₁`, and what "self-justification" means here

- `H` is `reflect` at the empty type, a constant of the language (R4 §1.3).
- `H₁` is a separate constant. It does not follow from `H` (Prop. 4.10′), while `H`
  follows from `H₁` at a constant budget (Prop. 4.10) **[R][K]**.
- Both are derivable because they are postulated, as Willard postulates his
  consistency axiom. What the metatheory proves is that postulating them is safe
  **[R][K]**:
  - read every type at a budget n;
  - by strong induction on n, every typed program has a value of its type;
  - in the `H`, `H₁` and `reflect` cases, the held certificate encodes a derivation at
    a budget smaller than n, so the hypothesis applies, and at ⊥ there is no value.

**Willard's two clauses.** The language states its own consistency, and it is
consistent. Clause (ii) is T1 **[R][K]**. Clause (i) is a reading of `H°`'s type,
which R4 §3.9 calls informal **[R]**. The statement is about certificates, not codes.

### 4.4 What runs and what only types

- `reflect` at a base type D ≠ 0 runs. Its ⊥-instance is `H`.
- `H` and `H₁` never run (Theorem 5.2). They let a program drop branches that are
  reachable only through a held refutation or a held contradiction.
- `Check` rejects every refutation and every contradictory pair anyway, with or
  without `H₁` (Corollary 3.7).

So `H₁` changes which programs type-check, not what any run does **[R][K]**.

## 5. Agents on λᶜᵉʳᵗ₀ as it stands

### 5.1 The agent type and the runner

```
Agent := Π(d :ω Syn). Π(self :ω Syn). Π(i :ω I). Π(r :₁ R).
           O ⊗ Σ(c :ω Syn). Σ(b :ω Bool). T(if b then chk c d else tt)
```

- **d and self.** The runner passes the code of the type `Agent` and the agent's own
  code. So the type never mentions itself, and no rule is added.
- **I** is a base data type.
- **r** is the round's supply of tokens.
- **The output** has type O. For safety take `O := Σ(a :₁ Nat). T(safe a)`, with
  `safe` closed and containing no `reflect`.
- **The successor part.** The agent computes `chk c d` at run time (c and d are then
  closed codes) and does case analysis on the result. A true verdict comes with
  evidence that c checks at d; false means "stop".

**The runner** (outside the language; trusted). In round k it:
1. decodes the current code into `Θₘ ⊢ t : Agent`;
2. runs t on (⌜Agent⌝, the code, the input, a supply built from k′ fresh tokens) in
   Θ_{m+k′}, so the supply's tokens are distinct from the agent's m;
3. if the verdict is true, re-checks the successor with the same `Check` and
   continues; otherwise it stops.

The runner has two policies:
- a **budget policy**, since a successor's text can declare any budget m′ and the
  runner would mint it: cap m′ + k′, or charge m′ against the supply;
- a **log** of every certificate a round builds, because `reflect` consumes them.

Only codes and data cross rounds.

### 5.2 The lineage theorem

**Hypotheses.**
- (h1) `Check(code₀, ⌜Agent⌝) = tt`, where ⌜Agent⌝ is the canonical code of a closed,
  well-formed type.
- (h2) The runner evaluates exactly the term decoded from the checked code, at
  budget m + k′, with distinct supply tokens.
- (h3) The literals passed are values of Syn and I.
- (h4) The runner's `Check` and the model's reading of `chk` are the same `Check`, the
  one Lemma 3.6 is proved for. In the implementation that requires ADR-0008's
  extracted `Check`.
- (h5) `safe` is closed and free of `reflect`.
- (h6) In every round the runner passes d = ⌜Agent⌝ and self = the current code, and
  on a true verdict the next round's code is exactly the returned c.

**Statement [A].** For every input sequence and every round k the runner reaches:
- round k's run terminates;
- its action satisfies `safe`;
- if its verdict is true, `Check(c_k, ⌜Agent⌝) = tt`, so round k + 1 again runs a
  checked agent.

**Proof plan** (the theorem is *pending* until P1 and P2 below are proved). By
induction on k:
1. CheckSpec decodes the code into a derivation with m < ‖code‖ **[K]**.
2. Lemma 3.6 puts t's value in Agent's semantic set. Applied to the literals and a
   supply in R's set at footprint k′ (the Π₁ clause needs m + k′ ≤ n), it gives:
   - an action whose `safe` holds;
   - a code c and a Boolean b;
   - non-empty evidence for `T(if b then Check(c, ⌜Agent⌝) else tt)`.
3. *Given P1 and P2,* Theorems 4 and 4′ make the runner's computed values the
   model's.

**Proof work that remains [O].**
- **P1, the projection lemma.** Theorem 4′ is stated for derivable programs of base
  type. The runner reads three projections: the action (`Nat`), the code (`Syn`),
  the verdict (`Bool`). Step 3 needs either derivations of the projection programs in
  Θ_{m+k′} (by weakening and typing the literals), or the Π-clause of 4′'s relation,
  which appears in 4′'s proof but not its statement.
- **P2, runner correctness.** The runner's operational steps (decode, build the
  supply, evaluate, re-check) implement the formal decoder, evaluator and `Check`
  under (h2) and (h6). This is to be met by extracting them (ADR-0008's scope).

**Trusted base.** `Check`, the decoder, the evaluator and the runner. The probes'
runner also re-types each round's application with the typer. The production runner
should evaluate the decoded derivation directly, with the literals in its
environment.

**The theorem is about the language.** Inside, the agent knows only that
`chk c d = tt`. For a successor given as code it cannot know more unconditionally
(§2.2). For a successor it holds as a certificate it can know more, through `reflect`,
if `reflect` works at the agent's type (Phase B; the toy's `reflA`).

### 5.3 What the probes show, and what they do not [E]

**Shown:**
- **Adoption by check** (`2026-11-13-agent-probe.clj`).
  - Agents type-check, with derivations of about 1,800 nodes.
  - An agent's verdict and the runner's re-check agree on a real successor, on junk,
    and on a type code.
  - A lineage in which each accepted code is decoded and run outputs 0, 2, 2, 2, and
    stops on junk.
- **A supply passed in** (`probe-supply-income.clj`).
  - The agent's derivation names no token: budget 0, 34,922 nodes.
  - With 40 tokens it pays for a certificate, `inspect`s it and `reflect`s it.
  - With 5 tokens it cannot pay, and it keeps itself.
- **`reflect` inside an adoption decision** (both reflect probes). The decision used
  the result of running a held certificate.

**Not shown by those two probes:** *evaluation of the candidate itself*. In both,
the reflected program is a score program the proposer supplied, unrelated to the
candidate, so the proposer chose the score. The gate below closes this.

**The gate (Phase A item 4).** The agent fixes a test input i. The proposer supplies
a derivation code D. The agent then:
1. checks D at the output's base type;
2. computes the expected root term, the candidate's term applied to i's literal,
   from the candidate's code;
3. compares it with D's root term by a code-equality function written in λᶜᵉʳᵗ₀
   (`rec-syn`);
4. only then pays for D and `reflect`s it.

No new rule and no derivation builder are needed. The cost is at least the
candidate's derivation plus the literal.

**The target term, exactly.**
- For a function candidate `t : I → D`, it is `app(t, ⌜i⌝)`.
- For an agent candidate, it is the action projection of `t ⌜Agent⌝ ⌜c⌝ ⌜i⌝ s₀`.
  Here c is the candidate's code, and s₀ is a supply term built inside the
  certificate from its own tokens; the agent checks every component but s₀.

**Gate soundness, a proof obligation [O]:** if `Check(D, ⌜D⌝) = tt` and
`termof(D)` equals the target, then `reflect` on a certificate of D returns the
target's value. It needs two lemmas:
- *extractor correctness:* `termof` returns the encoding of the term that the
  decoder reads from an accepted D;
- *the Reflect case of Lemma 3.6 with Theorem 4′.*

The context of D's root judgment is only tokens, so it constrains nothing else.

**The gate, run** (`probe-candidate-evaluation.clj`, 13 s) **[E]**. The candidate is
`λx. x + 2`; the test input is 3.
- With the proposer's derivation of "the candidate applied to 3" (66 nodes), the
  agent verifies the root term, pays from a 160-token supply, and gets **5**, the
  candidate's own output.
- With a derivation of the candidate applied to 4, of another function applied to 3,
  or of an unrelated `Nat` program, the root terms differ and it gets the fallback 0.

So in-run evaluation of a candidate's behaviour works in λᶜᵉʳᵗ₀ as it stands, at one
certificate per (candidate, input). Applying it to full agent candidates means
reflecting the base-type projections of an agent's output; that, and the sizes it
reaches, are Phase A work.

### 5.4 Costs found

- **A literal in program text** is recorded again at every ancestor node of the
  program's derivation. Certifying a program with a candidate's code as a literal
  exhausted a 1.5 GB heap. Candidates must travel as data.
- **Tokens named in program text** are recorded in every judgment's context: the
  derivation has 98,593 nodes at 40 tokens, against 34,922 with the supply passed in.
- **Re-typing every round** took 12–35 s and up to 1.7 GB for three toy rounds.

## 6. The time guard: toy level only

**What the toy checks [K].** The toy adds a type ▷A ("A next round") and
`become : code A → ▷A`.
- *Consistency:* the syntax is consistent, by induction on rounds, then budget.
- *Meaning:* with a round left, `become` of a code is that code's own reading one
  round later.
- *Conditionality:* "falsehood next round" is empty while rounds remain and inhabited
  when none do.

This is Fallenstein's T_κ (Yudkowsky and Herreshoff 2013, §4.2) as a typing rule
**[C]**.

**What it would give.** Internal, conditional statements about successors. It gives
trust that the next round's program has its type, never a result now. It is not
needed for safety (§5.2).

**What transferring it to λᶜᵉʳᵗ₀ needs [O].** The toy has no tokens, no `chk` and no
data codes: its codes are typed terms. The transfer needs:
- the rules for ▷ and `become`, with `become`'s premise `chk c ⌜A⌝`;
- `Check` extended to the new rules;
- the reading by rounds with Kripke-style functions;
- termination again.

## 7. What a self-improving agent gets

| Capability | Mechanism | Inside the agent? | Status |
| --- | --- | --- | --- |
| **Safe** in every round | agent type, runner, lineage theorem | no: a theorem about the language | ingredients [K]; induction [A]; P1 and P2 [O] |
| **Justified** adoption | the agent computes `chk c d` | yes | [E] |
| **Auditable** adoption | each acceptance has a derivation (35k–99k nodes in the probes) that anyone can re-check; the runner logs certificates | the record is outside | [E] with the current checker; [O] with the extracted one (ADR-0008) |
| **Self-extension** within a run | `reflect` on held base-type certificates | yes: complete; token cost known, time not | [R][K]; [E] for supplied programs |
| **Evaluation of received code** within a run, without self-justification | a step-limited evaluator written in λᶜᵉʳᵗ₀ | yes: any input, time-bounded, may run out of steps | [A]; not built |
| **Evaluation of a candidate** before adoption | `reflect` on the candidate's application, after root-term verification | yes | [E] for a small `Nat → Nat` candidate; [O] for agent candidates at scale |
| **Dead-branch elimination** | `H`, `H₁` | yes, at type-checking only | [R][K] |
| **Foresight** about successors | ▷, `become` | yes, conditional | [K] toy only |

**What is not claimed:**
- *Interpretability* beyond auditable records.
- *Coherence enforcement* beyond what `Check` already does.
- *Unconditional internal knowledge of the lineage.*
- *Stronger logic by self-certification*: G2.
- *Measuring one's own speed*: there is no clock.

## 8. Is this the right approach?

**For building safe self-improving agents:** typed hand-over with a fixed checker,
i.e. proof-carrying code. It is the simplest correct design, and Phase A builds it.
λᶜᵉʳᵗ₀ is a good host, because its ordinary layer computes on codes at full strength
and its checker is kernel-verified. Self-justification is not needed here.

**For the research aim**, what a running system can know and guarantee about itself
by itself, without an outside executor: λᶜᵉʳᵗ₀'s bounded local reflection is the
right object.
- Among the escapes in §3, it is the only one that is complete, internal, and
  accepts full-language candidates as data, with its cost in tokens known in
  advance.
- What it establishes is where sound self-trust stops (§0.5). Whether it also
  improves self-improvement is Phase D's hypothesis, and the prediction is that it
  does not beat the step-limited evaluator.

**Alternatives:**
- *No inspection* cannot accept candidates as data.
- *Fuel*, as a step-limited evaluator, is incomplete but time-bounded. For agent
  engineering it is the practical way to evaluate found code inside a run, and
  Phase A builds it.
- *A weaker-language interpreter* bounds what candidates may be.
- *Willard's arithmetic* restricts the whole system. λᶜᵉʳᵗ₀ restricts only
  certificates, and it does not compose certificates uniformly either (Thm. 4.6).
  Its codes, which agents manipulate, stay at full strength.
- *HOL with large cardinals* (Fallenstein and Kumar, ITP 2015) **[C]** buys
  conditional internal reasoning with set-theoretic assumptions. That is the time
  guard's role here, without them.

## 9. The plan

**Phase A — the agent layer.**
1. **Engineering gates:**
   - candidates as data;
   - the supply passed in, at budget m + k′;
   - a runner that evaluates decoded derivations directly;
   - the budget policy and the certificate log;
   - stated memory ceilings.
2. **The agent type** of §5.1, as a library definition, with a variant that passes
   state.
3. **The lineage theorem in Ansatz**, with hypotheses (h1)–(h6), the projection
   lemma P1, and runner correctness P2 (§5.2).
4. **The gate:** a probe in which the agent evaluates a candidate's own behaviour on a
   test input (§5.3). It is a go/no-go gate for Phase D.
5. **The step-limited evaluator** of §0.3, written in λᶜᵉʳᵗ₀, with its correctness
   proved and tested on the gate's candidates. It is the practical route for
   in-run evaluation, and Phase D's decisive baseline.
6. **Documents:** an R4 section and a tutorial section.

*Failure:* the theorem needs a rule change, or a metatheorem beyond the projection
lemma; or the gate is infeasible at practical sizes. Each is recorded with what it
costs the agent.

**Phase D — the experiment.**
- *Task:* candidate programs for a fixed scored task. The score is computed by the
  agent's own fixed scorer from the candidate's outputs on held-out inputs.
- *Proposer:* the same untrusted stream of candidates, with derivations, for every
  regime.
- *Regimes:*
  - (b1) adopt by check alone;
  - (b2) trial for a round, revert by state: safe because every candidate checks
    at the agent type, and revert needs the parent's code in the state;
  - (b3) hand over to a checked wrapper that runs the candidate and reverts: safe
    because the wrapper itself checks at the agent type;
  - (b4) evaluate in-run with the step-limited evaluator, then adopt;
  - (t) evaluate in-run with `reflect`, then adopt.
- *Metrics:* the score of the adopted program after N rounds; rounds run under worse
  programs; tokens and time spent.
- *Hypothesis:* (t) reaches a higher score, or wastes fewer rounds, than (b2), (b3)
  and (b4) at equal tokens and time.
- *Prediction:* (t) does not beat (b4). (b4) needs no per-input certificate and can
  reject slow candidates at a deadline; (t)'s only edge is that it never runs out
  of steps. A pass would be a surprise to explain.
- *Protocol,* to be fixed in writing before any run:
  - the candidate streams: a seeded generator, with the same seeds for every regime;
  - N, the per-round token budget, and how the runner measures time;
  - the decision rule: (t) passes only if it beats (b2), (b3) and (b4) on the
    pre-registered metric by a pre-registered margin, consistently across seeds; a
    tie counts as a failure.
- *Refuted* otherwise. Then self-justification adds nothing measurable to
  self-improvement in this architecture, and that is reported.

**Phase B (research): reflection beyond base types.** This means `reflect` at
function types, and at types `Π(x : D). T(b)` with b free of `reflect`. Lemma 3.4
fails for `T(b)` only because a `reflect` inside b makes its value depend on the
budget. Phase B asks how far sound self-trust extends to proofs of universal
statements. It is research on the limits of self-knowledge, not agent engineering.

**Phase C (optional): the time guard in the language** (§6). Take it up for the
research aim of internal foresight.

**ADR-0008's scope** should include the decoder, the evaluator and the runner path,
not only `Check`.

## 10. Status of every claim

| Claim | Status |
| --- | --- |
| No total universal function in a total language | [C] Brown–Palsberg §3.1 |
| No internal fuel bound works for all candidates | [A] |
| Trust in code at ⊥, with D1–D3 and a fixed point ⇒ ⊥ | [K] toy (`sViaQuine`, `sNoSoundReading`) |
| P5: λᶜᵉʳᵗ₀ derives no form of code consistency | [R] |
| □ evaluates at all types; no `A → □A`; no inspection | [C] Davies–Pfenning §2 |
| T1–T4 | [R][K] |
| T2's clause (i) as Willard's "states" | [R], informal |
| `reflect` runs held base-type programs at a smaller budget | [R][K] |
| No paradox from self-application through `reflect` | [R] (R4 §3.2) |
| The agent type needs no new rule | [E] |
| Lineage theorem | [A] over [K] ingredients; P1 (projections) and P2 (runner correctness) [O] |
| `reflect` used in an adoption decision | [E] |
| `reflect`'s token cost bounds certificate size, not running time | [R] (System T strength, R4 design) |
| A step-limited evaluator written in λᶜᵉʳᵗ₀ evaluates received code in-run without self-justification | [A]; not built |
| Self-justification adds a practical capability beyond typed hand-over and a step-limited evaluator | not shown; predicted not (Phase D) |
| Evaluation of the candidate's own behaviour | [E] small candidate (`probe-candidate-evaluation.clj`); [O] at agent scale |
| Time guard: consistency, meaning, conditionality | [K] toy; transfer [O] |
| `reflect` at first-order types | [O] |
| The list of escapes is exhaustive | not claimed |

## 11. Reviews

**Round 1** (2026-11-13) was two independent reviews of revision 1:
- Cursor's GPT-5.3 Codex Extra High:
  [`reviews/round1-cursor-gpt-5.3-codex-xhigh.md`](reviews/round1-cursor-gpt-5.3-codex-xhigh.md),
  with 11 findings, one critical;
- a Claude Opus subagent:
  [`reviews/round1-claude-opus-subagent.md`](reviews/round1-claude-opus-subagent.md),
  with 1 critical, 9 major and 9 minor findings.

All findings were accepted. The ones that changed the answer:

| Finding | What was wrong | Now |
| --- | --- | --- |
| Opus C1 | Candidate evaluation was claimed shown; the probes reflected a proposer-supplied score program | §5.3, §7: [O], with the gate in Phase A |
| Opus M9(d) | The draft did not consider that trial and wrapper adoption are safe without `reflect` | §0.4, §9: they are baselines; the claim for `reflect` is a falsifiable hypothesis |
| Opus M2 | It said self-justification's executable content is `reflect`, and that `H₁` comes from it | §4.3: `H` and `H₁` are postulates shown safe; `reflect` is bounded local reflection, with `H` as its ⊥-instance |
| Opus M6 | It claimed H₁ gives coherence | §4.4: `Check` already enforces it; H₁ only affects typing |
| Codex TSLC-01; Opus M4 | The lineage theorem used Theorem 4′ beyond base types, and its hypotheses were missing | §5.2: (h1)–(h5) and the projection lemma |
| Opus M1; Codex TSLC-05 | The diagonal-against-`reflect` steps were wrong | §4.2: the R4 §3.2 argument |
| Opus M3, M8 | The supply's budget and the free minting of successor budgets were unspecified | §5.1: Θ_{m+k′}, budget policy |
| Opus M5 | Size was said to be complete where fuel is not | §3: the difference is cost known in advance |
| Codex TSLC-02 | `reflect` was described as running any program | base types only |
| Codex TSLC-03; Opus m9 | "No external runner" | "no round-trip outside the run" (§0.3) |
| Codex TSLC-07; Opus M6 | "Verifiable" with an unextracted checker | "auditable", status split |
| Codex TSLC-08; Opus M5(c) | The time guard read as established for λᶜᵉʳᵗ₀ | §6: toy level only, transfer obligations |
| Codex TSLC-04; Opus M7 | "Exactly two options" and "the limit is sharp" | scoped; exhaustiveness not claimed |

**After round 1**, the gate probe was run and passed at toy size (§5.3).

**Round 4** (2026-11-14, on revision 3; one question): Cursor's GPT-5.3 Codex Extra
High, [`reviews/round4-cursor-gpt-5.3-codex-xhigh.md`](reviews/round4-cursor-gpt-5.3-codex-xhigh.md).
- **The question.** After round 3, I found that every revision had missed the
  obvious competitor to `reflect`: a step-limited evaluator written in λᶜᵉʳᵗ₀. I
  had also called token cost "predictable" as if it bounded time.
- **The review's verdicts:**
  - the evaluator is definable: yes in principle, but not built;
  - it needs no certificate and bounds time: holds;
  - tokens do not bound time: holds;
  - `reflect` is dominated: partly, because completeness and a total result type
    can matter;
  - "no practical capability": overstated;
  - Phase D lacked the decisive baseline: holds.
- **Applied:** §0.3–§0.5, §3, §7–§10. The practical advantages it listed (no
  out-of-steps branch, reuse of the proved evaluator, native speed, logged
  certificates) are stated in §0.3. The claim is now that no practical gain is
  shown and none is predicted, not that none exists.

**Round 3** (2026-11-13, on revision 3): Cursor's GPT-5.3 Codex Extra High,
[`reviews/round3-cursor-gpt-5.3-codex-xhigh.md`](reviews/round3-cursor-gpt-5.3-codex-xhigh.md).
- It found the round-2 findings resolved, except where §9 still listed (h1)–(h5).
  It added one major and two minor findings, all about consistency, and all fixed:
  - §9 item 3 now reads (h1)–(h6);
  - P2 is named in §7 and §10;
  - ADR-0009's context no longer says candidate evaluation is unshown.
- It checked the gate probe and found no false-accept path in its code equality. The
  root-term extractor's agreement with the decoder is the remaining proof
  obligation.
- **Its verdict:** "mostly correct and honestly scoped", and the plan "sound as a
  plan" and "acceptable for proceeding", with formal closure depending on P1, P2 and
  gate soundness.

**Round 2** (2026-11-13, on revision 2): Cursor's GPT-5.3 Codex Extra High,
[`reviews/round2-cursor-gpt-5.3-codex-xhigh.md`](reviews/round2-cursor-gpt-5.3-codex-xhigh.md).
- It judged most round-1 findings resolved, and five partly resolved. A Grok review
  was stopped after its connection to Cursor kept failing.
- It added six findings, five of them major. All were accepted:
  - the gate's target term was unspecified for agents: now "the target term,
    exactly" (§5.3);
  - root-term equality needed a soundness argument: the gate-soundness obligation;
  - the per-round runner invariants were missing: (h6);
  - the proof read as finished: it is now a proof plan, pending P1 and P2;
  - Phase D was not pre-registered: the protocol in §9;
  - the safety preconditions of b2 and b3 were missing, and "the only escape" was
    unscoped: both stated (§8, §9).
- Its verdict: no critical issue remains. The account cannot yet be accepted as
  complete end to end until the pending proofs P1 and P2 and gate soundness are done
  and Phase D is run. Those are exactly Phase A's and Phase D's work.
