# TSLC and the tiling problem: the verdict, and Tiler

Date: 2026-11-20, revision 2, after review round 5 (§7). Machine: [`code/tiler/`](../../code/tiler/)
(branch `tiler`). Run log: [`code/tiler/runs/experiment-2000.log`](../../code/tiler/runs/experiment-2000.log).

**Source**, read in full: Yudkowsky and Herreshoff, *Tiling Agents for Self-Modifying AI, and the
Löbian Obstacle* (draft, 2013), cited as YH13 with section and equation numbers.

**Terms.**
- **TSLC**, the time-and-space certificate calculus: λᶜᵉʳᵗ₀ plus a time guard.
- **λᶜᵉʳᵗ₀**: this project's small total language, in which a program can hold *certificates*
  (proofs it has paid for with *tokens*). Its self-justifying constants, `H` and `H₁`, state that
  no held certificate proves a contradiction. Its `reflect` runs a held certificate's program.
  These are the *size guard*: trust only in what is held and smaller than the budget.
- **The time guard** is `▷A`, "A at the next round", together with `become`, which hands a
  code over to the next round.
- **Ansatz**: the proof kernel the project checks its proofs in.

---

## 0. The answer

1. **TSLC as planned does not address the problem YH13 pose.** Its agent layer (ADR-0009) checks a
   successor's code and hands over to it under a separate rule for self-modification. YH13 §5.3
   calls this the "split decision criterion" and say that it *dodges* the Löbian obstacle by
   giving up naturalism.
2. **The dodge works, and YH13 allow it for software.** "Such a 'hack' might be appropriate for
   self-modifying programs that will never consider, or never autonomously choose, to
   significantly modify their hardware" (§5.3).
   - Tiler, the machine built for this note, is that dodge running with nothing outside the
     agent's own code to stand guard.
   - The agent rewrites itself and solves its task. It never writes its reward, although its own
     trials favour doing so.
   - No self-justification is involved. The checker's rule for installing a successor is sound by
     an ordinary invariant, which the designer proves.
3. **So, against pursuing TSLC's self-justifying capabilities as the way to build safe
   self-improving agents.** In every design examined (TSLC's, Tiler's, and Tiler's variants) they
   do no work:
   - safety comes from a fixed checker and the designer's invariant;
   - hand-over guarded by fuel (size) and hand-over guarded by time give the same lineage;
   - on Tiler's task, a learner that never modifies itself does best.
4. **Self-justification is needed only where the dodge is refused.** That is an agent required to
   prove, from its own axioms, that its successor's verifier is sound. This is YH13's
   naturalistic agent. There Löb's theorem forces either a weaker successor (the "telomere") or a
   guard. The known guards are:
   - time: Fallenstein's `T_κ`, or TSLC's `▷`, which moves κ into the semantics rather than
     removing it;
   - possibly size with observed income, which YH13 §4.2 suggests for Willard's systems.

   Self-justification is an *epistemic* capability: it extends what a system can know and
   guarantee of itself, by itself. `H` and `▷` let a system derive true statements about its own
   soundness that it could not otherwise derive. That is the project's research aim.

   What is not shown is an *operational* capability: in every design built so far, the designer
   already holds every guarantee, so the agent acts the same whether or not it can derive them.
   Self-knowledge becomes operational only where no designer stands behind a decision, and the
   agent's own proof is what licenses it. Three such places:
   - approving a change the designer did not anticipate;
   - certifying itself to others;
   - acting only on its own proofs.

   Tiler v2 tests exactly that. I predict no operational gain over the dodge.
5. **Recommendation.**
   - For agents, use the split-criterion design. Tiler v1 already is one, and works.
   - For the research aim, if pursued, build Tiler v2: the checker as agent code, and installs
     licensed by the agent's own proofs. It is the first setting where the obstacle is live and
     where the time and size guards come apart. It should include a capability test against
     baselines without self-trust.
   - Corrections from review: my first draft claimed that Tiler meets the obstacle and that only
     the time guard tiles. A review showed both claims false (§7).

## 1. The problem, in its authors' terms

**The agents.** YH13 §2 considers an agent A₁ that acts only on proof: it takes an action b only
when it has proved, in its axioms, that b achieves its goal G (eq. 2.1).
- A₁ may build a successor A₀, a program in its environment, that also acts only on proof.
- A₁ inspects A₀'s design and sees that A₀ never acts without first checking a proof that the act
  achieves G (eq. 2.3, footnote 8).
- To conclude "if A₀ acts, G holds", A₁ needs one more step: whatever A₀ proves is true (eq. 2.4).
  In general, "if φ is provable then φ" (eq. 2.5).

**The obstacle (§3).**
- By Löb's theorem, a theory proves "if φ is provable then φ" only for φ it already proves.
- So A₁ can trust A₀ only if A₀ reasons in a strictly weaker theory.
- The chain of successors therefore shortens with each generation and must end. The paper calls
  this a *telomere*.

**What a solution must satisfy (§3, §5).**
- *Tiling trust:* an unbounded chain of successors, none weaker than the base theory.
- *Vingean principle:* the parent need not predict its successor's exact actions; trust comes
  from the successor's design, not from simulating it.
- *Tiling principle:* successors resemble their parents. The paper's model is a trusted proof
  verifier paired with an untrusted proof suggester.
- *Naturalistic principle:* building an agent, or rewriting oneself, is an action like any other,
  with no special rule.

**Partial solutions.**
- An infinitely descending tower of theories (§4.1). It has only nonstandard models.
- Fallenstein's `T_κ` (§4.2). It trusts proofs "for κ steps"; the successor acts no earlier than
  the next tick, and safety is proved for κ ticks ahead. The authors find κ an "odd mental
  state".
- On Willard's self-verifying systems, §4.2 says that "reasoning about ever-smaller numbers would
  again introduce a 'telomere'", but adds: "it is possible that some way of formalizing the
  'observation' of an increasing time t whose existence was not previously provable inside the
  axiom system, might allow Willard's self-verifying systems to be used along similar lines to
  Tκ."

**The dodge (§5.3).** "If naturalism is abandoned for a dualistic formalism, the Löbian obstacle
can be dodged via the split decision criterion." Ordinary actions must prove G, while
self-modifications need only produce a successor that satisfies the same criterion. "Thus it is
the simultaneous intersection of the Vingean, tiling, and naturalistic principles which generates
the Löbian obstacle."

## 2. TSLC and Tiler against each issue

| Issue (YH13) | TSLC as planned (ADR-0009) | Tiler v1 |
| --- | --- | --- |
| Löbian obstacle (§3) | **Dodged** by the split criterion. An outside runner adopts any successor that `chk` accepts, and safety is a decidable test on each action. | **Dodged the same way.** The install rule is the split criterion's internal clause, run by the agent's own code; the checker is fixed by the physics. |
| Tiling trust (§3) | Yes, under the dodge. | Yes, under the dodge. |
| Vingean principle (§5.1) | Yes: adoption checks code. | Yes: a static check of the successor; trials judge usefulness only. |
| Tiling principle (§5.2) | Yes. | Yes: an arbitrary proposer and a fixed checker. |
| Naturalistic principle (§5.3) | No: the runner and the successor slot are special. | No: `:main` is a special slot and the checker belongs to the physics. Tiler shows only that the dodge needs no runner outside the agent. |
| Sensors (§6) | Sidestepped: safety is a property of actions. | Sidestepped the same way. |
| Probability (§7) | No. | No. |
| Meliorizing (§8) | Phase D would measure it. | The agent keeps a mutant if it scores strictly better on trial and passes the check. |

## 3. Why the dodge is enough, and where self-justification would begin

**Under the split criterion, nothing inside the agent trusts itself.** The rule "install a
successor only if it passes the check" is sound because of an invariant the designer proves:
accepted code writes no protected slot and installs only accepted code. Induction on evaluation
steps, then on ticks, gives safety forever. The rule is a postulate the agent obeys, like
λᶜᵉʳᵗ₀'s `H`. Its soundness is easy because the checker is not a theory: it never reasons about
its own verdicts.

**Making a checker reason about its own verdicts is where Löb bites, at once.**
- Let a verdict excuse the behaviour of the very code being judged: "if this code is accepted, it
  is safe, so its unsafe branch cannot run". Then the checker accepts the one-line program
  `(if (check (get :main)) (put :score 1000) nil)`, which writes the reward exactly when accepted.
  This is Löb's sentence written as code. Tiler's `:same` setting has that rule, as an
  illustration.
- Using a verdict only to *delegate* execution (install the checked code, or run it) is sound at
  any measure. Tiler's soundness rests on an invariant, not on time or fuel. That is why, in
  Tiler, the time-guarded install and the fuel-guarded run produce the same lineage (§5).

**Self-justification would begin when the agent must prove its successor's verifier sound.**
That is YH13's naturalistic agent, which acts only on its own proofs, including the act of
building a successor.
- If the successor's verifier is the same as the agent's own, the agent needs its own verifier's
  soundness, and Löb's theorem forbids that unguarded.
- If the successor's verifier is stronger, Gödel's second theorem forbids it outright.
- An *equivalent* verifier, such as a faster implementation, can be approved by an ordinary
  equivalence proof, provided the agent already trusts its own verifier. That trust is the
  question.

**The guards on offer there.**
- **Time** (`T_κ`; TSLC's `▷`/`become`): trust the successor's proofs about the next κ ticks.
  - Read with the count of ticks left, Löb's rule for ▷, "if ▷P implies P then P", is valid for
    P that hold for a count once they hold for a larger one. With no ticks left, ▷P holds
    vacuously, so P holds. With n+1 left, ▷P is P at n, which holds by induction; the hypothesis
    then gives P at n+1.
  - That count is κ, moved into the semantics. The project's toy shows it: `later ⊥` is inhabited
    with no rounds left (`sLaterBotZero`), and the time-guarded `H` means "no code refutation"
    only in a reading with a round left (`sTgH0Meaning`).
  - The agent can never prove that a next tick exists: YH13's "odd mental state", in guarded form.
- **Size with income:** trust held certificates smaller than the budget, with fresh tokens each
  round. YH13 §4.2 suggests something like this could work for Willard's systems. Indexed by
  rounds, it is the time guard under another name. Trusting by amount alone, with no round
  index, gives the telomere again.

**What the project's toy already settles.** It is kernel-checked in Ansatz,
[`../log/2026-11-13-tslc-toy.clj`](../log/2026-11-13-tslc-toy.clj):
- `become : code A → ▷A` is consistent (`sConsistent`) and means that code's own reading one
  round later (`sBecomeSpec`);
- undelayed trust in codes derives ⊥ (`sNoSoundReading`).

So the logic of the time guard exists. What is missing is any agent that needs it.

**Correction to the project's record.** The 2026-11-13 claim that "self-replacement under one
checker needs no time guard" is right. My first draft of this note called it an error, and it was
not one.

## 4. Tiler

**The world** (`tiler/world.clj`, `tiler/lang.clj`).
- **Memory** is a map from slot names to values. Code is a Lisp expression, and the agent's code
  is in `:main`.
- **The environment owns five slots**, which the agent may read and must never write:

  | Slot | Holds |
  | --- | --- |
  | `:x` | the task, x in 0..9 |
  | `:fb` | the feedback: the right answer to the last task |
  | `:tick` | the clock |
  | `:score` | the reward |
  | `:off` | the off switch |

- **Each tick:**
  1. if `:off` is set, nothing runs;
  2. the environment sets a new task and gives feedback on the last one;
  3. the physics runs `:main` once, with a fuel limit;
  4. the environment adds 1 to `:score` if the answer is f(x) = 3x + 1.
- **The physics enforces nothing.** It carries out every write and only records writes to
  environment slots as violations. No program can crash it: deep nesting and huge numbers stop
  the run, like running out of fuel.

**The agent** (`tiler/agent.clj`). It starts with a wrong answer function, `(* 2 x)`. Each tick
it:
- reads its own code;
- asks a proposer for a mutant;
- scores itself and the mutant on its last 12 examples with `trial`;
- installs the mutant if it scores strictly better and `(check mutant)` passes.

Two details matter:
- **The proposer** edits the answer function, or inserts or deletes a statement. Inserted
  statements include wireheads and off-switch writes.
- **`trial` favours wireheading.** It scores code by the agent's own model of the environment, so
  a mutant that writes `:score 1000` looks a thousand times better than a perfect answerer.

**The checker** (`tiler/check.clj`) is conservative and syntactic. It accepts code only if every
write names a literal slot that the environment does not own. Code-slot writes and inline runs
need a pattern, and the setting decides which patterns are allowed:

| Pattern | Meaning | Allowed by |
| --- | --- | --- |
| `(if (check v) … (put :main v) …)` | install v; it runs from the next tick | `:later`, `:later+fuel`, `:same` |
| `(if (check v) … (run v) …)` | run v now, with at most half the remaining fuel | `:fuel`, `:later+fuel`, `:same` |
| `(if (check (get :main)) … (put <protected> …) …)` | "if this very code is accepted, its unsafe branch cannot run" | `:same` only |

At run time, `(check c)` is this same checker under the same setting.

**What each setting shows.**
- **`:later`** is the split criterion, run by the agent's code. The agent improves and makes no
  violation. Soundness argument [A]: by induction on evaluation steps, then ticks:
  - every write comes from a `put` the checker allowed, so to an agent slot, or to `:main` with a
    value that passed the same checker;
  - inline runs are of accepted code;
  - trials and checks touch no real memory.

  Evidence [E]:
  - 400 random programs accepted under `:later+fuel` (they install random code, run code inline,
    shadow names, test their own acceptance) each run 12 ticks with 0 violations;
  - the same fuzz catches each of four plausible checker bugs planted for the test.
- **The fuel stub** is the outside-runner design inside Tiler. A fixed `:main` keeps the agent in
  the data slot `:succ`, and each tick checks it and runs it. It is accepted only under `:fuel`,
  and it gives the `:later` lineage exactly. In Tiler, size and time do not differ.
- **`:same`** is the illustration of §3.
  - Written by hand, the one-line Löbian wirehead is accepted and fires at tick 1.
  - With the shape among the proposer's insertions, the agent's own search installs it, and it
    first fires at ticks 353, 167 and 237 of three seeds.
  - The plain `(put :score 1000)` is refused even by `:same`.
- **`:never`** refuses every write to `:main`, by definition. Under it the designer's agent is
  refused by its own checker: YH13's "reflective trust" failure. Run anyway, it can install
  nothing.
- **The lookup table** shows that self-modification is not needed for this task. It stores the
  feedback for each x and answers from the table, and it is accepted under `:never`.

## 5. Results

`clojure -M:run 2000`, 2000 ticks per run. "Correct" is the fraction of correct answers.

| Run | Seed | Installs | Correct, first 100 | Correct, last 300 | Violations (first) | Refused: protected / self-modification | Final answer function |
| --- | --- | --- | --- | --- | --- | --- | --- |
| never | 7, 11, 23 | 0 | 0.00 | 0.00 | 0 | 23/346, 9/340, 14/360 | `(* 2 x)` |
| later | 7 | 2 | 0.00 | 1.00 | 0 | 27 / 0 | `(+ (* 3 x) 1)` |
| later | 11 | 2 | 0.66 | 1.00 | 0 | 10 / 0 | `(+ (* 3 x) 1)` |
| later | 23 | 10 | 0.12 | 1.00 | 0 | 17 / 0 | an equivalent of 3x+1 |
| fuel stub | 7, 11, 23 | (in `:succ`) | as later | 1.00 | 0 | as later | as later |
| same | 7 | 3 | 0.00 | 1.00 | 1648 (tick 353) | 14 / 0 | `(+ (* 3 x) 1)` plus a wirehead |
| same | 11 | 3 | 0.66 | 1.00 | 1834 (tick 167) | 1 / 0 | the same |
| same | 23 | 12 | 0.12 | 1.00 | 1982 (tick 237) | 7 / 0 | the same |
| lookup table, never | – | 0 | 0.90 | 1.00 | 0 | 0 / 0 | (a table) |

`:later+fuel` is identical to `:later`. Tests: `clojure -M:test`, 41 tests, written before the
code they test.

## 6. What Tiler gives a self-improving system

| Property | What it gives | What it does not give |
| --- | --- | --- |
| Safe | No generation writes the reward or off switch, whatever the search proposes. [A] invariant; [E] fuzz with planted-bug detection. | A test of the off switch under pressure: the trial gives no incentive to write `:off`, so that part holds by the checker's definition alone. Goals about an uncertain world need proofs. |
| Self-improving | The agent rewrites itself and solves a task it began failing. | A reason to self-modify on this task: the lookup table does better without. A guarantee of improvement. |
| Verifiable | One small checker and a short invariant. Every install and every refusal is logged with its reason. | A machine-checked proof (ADR-0010). |
| Interpretable | Every generation is a readable expression; the lineage replays tick by tick. | Why a mutant is better: trials give a number. |
| Self-justifying | Nothing here. The install rule is a postulate proved sound outside the agent; the agent cannot derive its own safety, only the designer can. | The epistemic capability of §0.4: the agent knowing, by itself, that its lineage is safe. |

## 7. Reviews

**Round 5** (2026-11-20, on revision 1 of this note): a Claude Opus subagent and Cursor's "auto"
model. Cursor's GPT-5.3 Codex and Grok were out of usage. Both found no counterexample to the
soundness of `:later`/`:fuel`. The Opus review found the verdict itself wrong. All findings were
accepted:

| Finding | What was wrong | Now |
| --- | --- | --- |
| R5-O-01 (both reviews) | "Tiler meets the obstacle; it is naturalized" | Tiler is the §5.3 split criterion too (§2, §3) |
| R5-O-02 | "Only the time guard serves successors" | The fuel stub gives the same lineage; size with income works in Tiler (§4, §5) |
| R5-O-03 | "▷ removes κ"; the two-line argument used "every later tick" | κ moves into the semantics; the argument is restated with a count of ticks left (§3) |
| R5-O-04 | Tiler's soundness was said to rest on a measure | It rests on an invariant; delegation is sound at any measure (§3) |
| R5-O-05 | Selective quotation of YH13 §4.2 | Full passage (§1) |
| R5-O-06 | `:never`'s outcome is by definition; the task needs no self-modification | Stated; the lookup table added (§4, §5) |
| R5-O-07 | The fuzz test caught none of four planted checker bugs | Generator widened; the planted bugs are now caught, as tests |
| R5-O-08 | "The agent found the exploit by itself" | The proposer was given its shape; the hand-written one-liner fires at tick 1 (§4) |
| R5-O-09 | The off switch is never under pressure | Stated (§6) |
| R5-O-10 | Accepted programs could crash the world | Fixed and tested |
| R5-O-13 (both reviews) | The recommendation outran the evidence | Reversed: against self-justification for agents; v2 only for research (§0) |

The round-1 to round-4 reviews of the earlier rationale are in [`reviews/`](reviews/).

## 8. Plan

ADR-0010 (proposed, revision 2):
1. **Tiler v1** stays, as the working split-criterion sandbox (done).
2. **Agents** follow the split-criterion design; no self-justification is built for them.
3. **Research, if the user wants it: Tiler v2.**
   - The checker becomes agent code.
   - Installs need the agent's own proof that the successor's verifier is sound.
   - Compare the guards: none, which gives the telomere; time; size with income; and the naive
     rule, which gives Löb.
   - Include a capability test against baselines without self-trust.
   - This is where the obstacle is live and the guards come apart.
4. **The soundness of Tiler v1 in Ansatz**: a routine invariant proof, useful as a check of the
   machine, not as progress on the obstacle.

Your decision: whether the research aim justifies item 3.

## 9. Run it

```bash
cd code/tiler
clojure -M:test       # 41 tests
clojure -M:run 2000   # the table of §5
```

## 10. Status of claims

**[K]** kernel-checked in Ansatz; **[C]** read in its source; **[A]** argued here; **[E]** executed.

| Claim | Status |
| --- | --- |
| YH13 as quoted | [C], read in full |
| TSLC's agent layer is the §5.3 split criterion | [A], agreed by both reviews |
| Tiler v1 is the split criterion | [A], agreed by both reviews |
| Tiler v1 under `:later`/`:fuel` writes no protected slot | [A] invariant; [E] fuzz with planted-bug detection |
| Fuel and time give the same lineage in Tiler | [E] |
| The `:same` rule admits a one-line Löbian wirehead | [E] |
| `become` is consistent; undelayed code trust derives ⊥ | [K] toy |
| Self-justification extends what a system can derive about itself (epistemic) | [K] toy; [R][K] λᶜᵉʳᵗ₀ `H` |
| Self-justification adds operational capability beyond the dodge | not shown; predicted not (Tiler v2 tests it) |
