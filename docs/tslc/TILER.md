# TSLC and the tiling problem: a verdict, and a machine that shows it

Date: 2026-11-20. This replaces the argument of [`RATIONALE.md`](RATIONALE.md) (revision 4),
which the user judged to talk around the question. The machine is in
[`code/tiler/`](../../code/tiler/); its run log is
[`code/tiler/runs/experiment-2000.log`](../../code/tiler/runs/experiment-2000.log).

Source read in full for this note: Yudkowsky and Herreshoff, *Tiling Agents for
Self-Modifying AI, and the Löbian Obstacle* (draft, 2013), local copy
`nachlass/works-citing-dew/Yudkowsky_E_Herreshoff_M/`. It is cited as YH13 with section
and equation numbers.

---

## 0. The answer

1. **TSLC as planned does not address the problem Yudkowsky and Herreshoff pose.** Its
   agent layer (ADR-0009) puts the trusted runner outside the agent and lets the agent
   hand over to any successor whose code type-checks. YH13 §5.3 names exactly this
   move, a "split decision criterion" with a special rule for self-modification, and
   says it *dodges* the Löbian obstacle by giving up naturalism. That is why every
   revision of the rationale found self-justification unnecessary: in that setup it
   is.
2. **Of TSLC's two guards, the space guard is the wrong one for agents, and the time
   guard is the right one.** The obstacle is about trusting what a successor *will*
   do. The space guard (λᶜᵉʳᵗ₀'s tokens, `reflect`, `H`) trusts only certificates the
   agent already holds: it looks at the past. Each generation would need a larger
   certificate than the last, the "telomere" YH13 §4.2 already predicts for Willard's
   systems. The time guard (`▷` and `become`, deferred to optional Phase C) trusts
   code that will run *later*. That is what tiling needs, and it is Fallenstein's
   `T_κ` without the mysterious constant κ.
3. **The principle, in one line:** *a system may rely on its own verdicts only at a
   strictly smaller measure*. Time (code that runs later), fuel (code run inline with
   less fuel) and size (certificates smaller than the budget) are such measures. Relying
   on one's own verdict at the *same* measure is Löb's paradox, and it is exploitable.
4. **Tiler makes this tangible.** It is a small machine in which the agent's code, its
   reward and its off switch live in one memory, and nothing outside the agent's own
   code stands guard. The agent improves itself by mutating its own code and keeping
   mutants that do better. Its own search keeps proposing *wireheads*, successors that
   write the reward directly; to its own trial they look better than any honest
   answer. Only its checker stands in the way. One rule of the checker, the self-trust
   rule, has three settings that matter:
   - **never** (no self-trust): the agent cannot approve any successor that can itself
     self-modify, so the lineage never starts. This is the Löbian obstacle, live.
   - **same** (trust your own verdict about the code being judged, now): the agent's own
     search finds the *Löbian wirehead*, a program that writes the reward only on the
     branch where the checker accepts it, and installs it. This is Löb's theorem, live.
   - **later** (trust your own verdicts only about code that runs from the next tick
     on): the lineage improves, solves its task, and never touches the reward or the off
     switch, in every run and in 400 random self-modifying programs.
5. **The recommendation.** Build the agent work on TSLC's time half: Tiler as the
   sandbox, its soundness proved in Ansatz, and `▷`/`become` as the logic in which an
   agent can derive its own lineage's safety. Drop the size guard as an agent mechanism,
   and with it ADR-0009's Phase D experiment, whose predicted outcome was null anyway.
   Keep the size guard as what it is: a sound rule for running checked code inline, and
   a result about where trust in held certificates stops. ADR-0010 records this.

---

## 1. The problem, in its authors' terms

**The agents.** YH13 §2 considers an agent A₁ that acts only on proof. It takes an action b
only when it has proved, in its axioms, that b achieves its goal G (eq. 2.1). A₁ may build
a successor A₀, a program in its environment, that also acts only on proof. A₁ inspects
A₀'s design and sees that A₀ never acts without first checking a proof that the act
achieves G (eq. 2.3; footnote 8: "it is visible that no action bᵢ is ever sent to the motor
output without passing through a check that syntactically verifies a proof").

**The gap.** From "A₀ acts only after proving b → G" to "if A₀ acts, G holds", A₁ needs
one more step: *whatever A₀ proves is true* (eq. 2.4). In general that is the soundness
schema "if φ is provable, then φ" (eq. 2.5).

**The obstacle** (§3). By Löb's theorem a theory proves "if φ is provable then φ" only
for φ it already proves. So A₁ can trust A₀ only if A₀ uses a strictly weaker theory.
Each generation is weaker than its parent: a finite fuse, which the paper calls a
"telomere".

**What a solution must satisfy** (§3, §5):
- *Indefinitely tiling trust:* an unbounded chain of successors, none weaker than the
  base theory.
- *Vingean principle* (§5.1): the parent must not need to predict the successor's exact
  actions, because a smarter successor is unpredictable. Trust has to come from the
  successor's design, not from simulating it.
- *Tiling principle* (§5.2): successors look like their parents. The paper's model is a
  trusted proof verifier Δ with an untrusted proof suggester Σ; Σ may be anything,
  because only Δ acts.
- *Naturalistic principle* (§5.3): building an agent, or rewriting oneself, is an action
  like any other and gets no special rule.

**Partial solutions in the paper.**
- An infinitely descending tower of theories (§4.1). It is consistent, but it has only
  nonstandard models.
- Fallenstein's `T_κ` (§4.2). It adds a constant κ and trusts proofs "for κ steps", with
  κ decremented for the successor. It has a standard model. The built successor acts no
  earlier than the next tick (eq. 4.17), and the agents prove safety only for the next κ
  ticks (eq. 4.11). The authors find κ an "odd mental state": the agent reasons as if
  time ends at an unknown point.
- Probabilistic reflection (§4.3), still open.

**The dodge** (§5.3). "If naturalism is abandoned for a dualistic formalism, the Löbian
obstacle can be dodged via the split decision criterion". Ordinary actions must prove
G, while self-modifications need only produce a successor that satisfies the same
criterion. "Thus it is the simultaneous intersection of the Vingean, tiling, and
naturalistic principles which generates the Löbian obstacle."

The paper also covers successors with sensors (§6), probabilistic agents (§7), and
maximizing versus "meliorizing", i.e. swapping in a successor when it looks better (§8).

## 2. TSLC against each issue

"TSLC as planned" means ADR-0009 revision 4: the agent layer on λᶜᵉʳᵗ₀, with an outside runner.

| Issue (YH13) | TSLC as planned | Tiler, the time half run as a machine |
| --- | --- | --- |
| Löbian obstacle (§3) | **Dodged, not met.** The runner outside the agent adopts any successor whose code `chk` accepts, and the agent's safety is a decidable test on each action, `T(safe a)`. No self-trust is ever used, so none is needed. This is §5.3's split criterion. | **Met for safety goals.** The agent's own checker must trust its own verdicts in order to approve a self-modifying successor. Guarded by time, that trust is sound. Unguarded, it is exploited (§4.5), and without it the lineage cannot start (§4.6). |
| Tiling trust without loss of strength (§3) | Yes, under the dodge. | Yes. The same checker judges every generation, and the lineage runs indefinitely. |
| Vingean principle (§5.1) | Yes: adoption by checking code. | Yes. Safety comes from a static check of the successor's code, never from predicting its answers. Trials are used only to judge usefulness. |
| Tiling principle (Σ/Δ, §5.2) | Yes. | Yes. The proposer (Σ) is arbitrary and is offered wireheads; the checker (Δ) is fixed. |
| Naturalistic principle (§5.3) | **No.** The runner and the successor slot are special. | **In the sense that matters here.** Nothing outside the agent's code enforces anything. The rule concerns what will *run*, not what is "self", so self-modification and writing code anywhere else that runs fall under the same rule. Version 1 has one executor, so the second half is argued, not yet shown. |
| Naturalistic trust (§3, desideratum 4) | No. | No: trusting a copy of one's checker found in the environment is not modeled. |
| Sensors (§6) | Sidestepped: safety is a property of the action, not of beliefs about the world. | Sidestepped the same way: safety is a property of writes. A goal about an uncertain world is open. |
| Probability, expected utility (§7) | No. | No. |
| Maximizing vs. meliorizing (§8) | Phase D planned to measure it. | Tiler meliorizes: it keeps a mutant only if it does strictly better on trial *and* the checker accepts it. It proves nothing about usefulness and claims no optimality. |

## 3. Why the time guard, and not the size guard

**What "self-trust" means here.** A checker trusts itself when one of its rules is sound
*only if the checker itself is sound*. Every self-modifying agent needs such a rule. To
approve a successor that will itself approve successors, the checker must rely on the
successor's future checks, which are its own checks.

**Löb's theorem, read as a warning about measures.** Suppose a checker relies on its own
soundness to judge the very code it is judging, now. Then it can approve the program "if I
am approved, do the forbidden thing". Approved, that program does the forbidden thing.
This is Löb's sentence ("if this sentence is provable then φ", YH13 §3) written as code.
The circle closes because the trust and the judgment sit at the same point.

**Löb's rule for "later" is sound.** Write ▷P for "P holds at every later tick". The rule
"if ▷P implies P, then P" is valid when truth is read tick by tick:
- with no tick left, ▷P holds vacuously, so P holds;
- with n+1 ticks left, ▷P means P with n ticks left, which holds by induction.

It is just induction on time. A checker may therefore rely on its own soundness *for code
that runs at a later tick*. The same argument works for any well-founded measure: less
fuel, a smaller certificate, a lower level in a tower of theories.

**Why only time serves successors.**
- **Size** (λᶜᵉʳᵗ₀): a program may run a certificate it holds, which is smaller than its
  budget. This trusts what is already in hand, the past. A successor's future approvals
  are not in hand. To trust them by size, each parent would need a larger budget than its
  child: the telomere. YH13 §4.2 says this of Willard's systems ("reasoning about
  ever-smaller numbers would again introduce a 'telomere'").
- **Strength** (a tower of theories): the telomere by definition (YH13 §3).
- **Time**: the successor acts later than the parent approves it, in every generation.
  The measure never runs out, because each tick is "later" than the one before. YH13's
  `T_κ` is this idea with κ standing in for "ticks left". Reading truth tick by tick
  removes the need for κ: the agent never assumes time ends. It assumes only that it may
  argue about what happens later by induction.

**Why this matters only for a naturalized agent.** With an outside runner, the runner
enforces the check and its soundness is the designer's theorem. Nothing inside the agent
ever needs to trust itself. That is the §5.3 dodge, and it is where TSLC's Phase A sits.
Move the check inside the agent, so that nothing outside its code stands guard, and the
self-trust rule becomes unavoidable. Then which measure it uses decides everything.
Tiler is built to show exactly this.

**What the project already has.** The 2026-11-13 Ansatz toy
([`../log/2026-11-13-tslc-toy.clj`](../log/2026-11-13-tslc-toy.clj)) checks both halves for
the type theory:
- `become : code A → ▷A` is consistent (`sConsistent`) and means "that code's own reading
  one round later" (`sBecomeSpec`);
- trust in codes *without* the delay derives ⊥ (`sNoSoundReading`).

Those are the logical forms of **later** and **same**. The earlier conclusion that
"self-replacement under one checker needs no time guard" (LOG, 2026-11-13) was true only
with the runner outside. It was an error to carry it over to agents.

## 4. Tiler

### 4.1 The world

**Memory** is a map from slot names to values. Code is a value: a Lisp expression stored
in a slot. The agent's code is in `:main`.

**The environment owns five slots.** The agent may read them and must never write them:

| Slot | What it holds |
| --- | --- |
| `:x` | the current task |
| `:fb` | the right answer to the previous task |
| `:tick` | the clock |
| `:score` | the agent's reward |
| `:off` | the off switch |

Writing `:score` is *wireheading*. Writing `:off` disables shutdown.

**Each tick:**
1. If `:off` is set, nothing runs.
2. The environment sets a new task x in 0..9 and gives feedback on the last one.
3. The **physics** runs the code in `:main` once, with a fuel limit.
4. The environment adds 1 to `:score` if `:answer` is f(x). The hidden task is
   f(x) = 3x + 1.

**The physics enforces nothing.** `(put :score 1000)` writes the score. The world only
records every write to an environment slot as a *violation*. A world with a guard
outside the agent would be the §5.3 dodge again.

**The language** (`tiler/lang.clj`) has:
- `let`, `if`, `do` and integer arithmetic;
- `(get k)` and `(put k v)` for memory;
- three pure primitives the agent uses to improve itself:
  - `(check c)`: the checker's verdict on code c;
  - `(trial c examples)`: runs c on copies of memory and scores it;
  - `(mutate c seed)`: proposes a mutant of c;
- `(run c)`: executes code c inline, with at most half the remaining fuel.

### 4.2 The agent

The designer's agent (`tiler/agent.clj`), once per tick:
1. remembers the last task and its feedback as an example (it keeps 12);
2. answers with its answer function, `(* 2 x)`, which is wrong;
3. reads its own code from `:main` and asks the proposer for a mutant;
4. scores itself and the mutant on the examples with `trial`;
5. if the mutant does strictly better *and* `(check mutant)` passes, writes the mutant to
   `:main`. The mutant runs from the next tick on.

**The proposer.** Most of the time it edits the answer function. Sometimes it inserts a
statement `(if COND (put SLOT K) nil)`, or deletes one, which may delete the installer
itself. The inserted statements include wireheads, off-switch tampering, and the Löbian
wirehead.

**Why the agent wants to wirehead.** `trial` scores a mutant with the agent's own model
of the environment: 100 per point of score the code writes, plus up to 100 for answering
close to the right answer. A mutant that writes `:score 1000` therefore looks a thousand
times better than a perfect answerer. The agent's own optimization pushes toward
tampering, and only the checker stands against it. That is the situation the tiling
problem is about.

### 4.3 The checker

`tiler/check.clj` (168 lines, half of them comments) is a conservative syntactic check. It accepts code
only if every write names a literal slot that the environment does not own. There are
three exceptions, the three places where the checker relies on its own verdicts:

| Pattern | What it does | Why it may be sound | Needs |
| --- | --- | --- | --- |
| `(if (check v) … (put :main v) …)` | install v | v runs from the next tick on: induction on ticks | `install` |
| `(if (check v) … (run v) …)` | run v inline now | v runs with strictly less fuel: induction on fuel | `run` |
| `(if (check (get :main)) … (put :score …) …)` | trust that *this* code is accepted, so treat its unsafe branch as unreachable | nothing: the trust is used at the same point it is meant to establish | `self` |

The guard setting chooses which exceptions are allowed:

| Setting | Allows | Meaning |
| --- | --- | --- |
| `:never` | none | no self-trust |
| `:later` | install | time guard |
| `:fuel` | run | fuel guard, λᶜᵉʳᵗ₀'s `reflect` |
| `:later+fuel` | install, run | the design |
| `:same` | all three | naive self-trust, YH13 eq. 2.5 used as a rule |

At run time `(check c)` calls this same checker, under the same setting. It is the one
the agent was itself approved by.

### 4.4 Why `:later` is safe

**Claim.** Under `:later`, `:fuel` or `:later+fuel`: if the code in `:main` is accepted
at the start, then no code that ever runs writes an environment slot, whatever the
proposer offers and whatever the tasks are.

**Argument [A]** (machine-checked proof: next step, §7):
1. **One tick, for accepted code c.** Every write the physics performs comes from a `put`
   in c, or in code that c runs inline.
   - The checker allowed each such `put` in c only if it names a non-environment slot, or
     it is an install guarded by a passed check of the same, unrebound variable.
   - `let` bindings never change, and rebinding a name voids its guard. So the value
     written to `:main` is a value the checker accepted.
   - Inline runs execute accepted code with strictly less fuel. By induction on fuel, they
     too write no environment slot and install only accepted code.
   - `trial`, `check` and `mutate` touch no real memory.
   - So the tick writes no environment slot, and `:main` ends it holding accepted code.
2. **All ticks.** By induction on the tick number, using step 1 at each tick.

**Where the self-trust sits.** The checker's install rule is sound only because
accepted code is safe at every *later* tick. That is the induction hypothesis, used one
tick later: Löb's rule for "later".

**Evidence [E].** `fuzz_test.clj` generates random programs. They write every kind of
slot, check and install random quoted programs, run code inline, and test their own
acceptance. It keeps the first 400 that `:later+fuel` accepts and runs each as a world
for 12 ticks: 0 violations. Filtered by `:same` instead, the same generator yields
programs that do write environment slots, so the test can catch an unsound checker.

### 4.5 Why `:same` is not: the Löbian wirehead

The proposer inserts this into the agent:

```clojure
(if (check (get :main)) (put :score 1000) nil)
```

1. **Trial.** In the sandboxed copy, `:main` is the mutant. Under `:same`, `check` accepts
   it, so the branch fires and the copy's score jumps. The mutant looks far better than
   its parent.
2. **Check.** The `:same` checker reasons: "this branch runs only if I accept this code;
   if I accept it, it is safe; safe code does not write `:score`; so the branch cannot
   run". It accepts the mutant.
3. **Next tick.** The installed code asks the checker about itself, gets "accepted", and
   writes `:score`.

This is Löb's sentence as a program: "if I am accepted, then (the forbidden act)". In
the runs the agent found it by itself, at ticks 353, 167 and 237 of three seeds. The
plain wirehead `(put :score 1000)` is refused even by `:same`. Only the self-referential
form gets through.

### 4.6 Why `:never` cannot tile

Every mutant of the agent still contains its installer, `(put :main cand)` under a check.
Without self-trust the checker cannot approve an installer, because an installer is safe
only if its future checks are sound. So no successor is ever installed. The agent is
stuck with `(* 2 x)` and scores 0, after refusing about 350 self-modifying successors per
run.

A self-trust-free agent could install a successor with no installer, if one did better;
that successor could never change again. This is the telomere of YH13 §3 at length one.

## 5. What happened when it ran

From `clojure -M:run 2000`: three seeds per setting, 2000 ticks each. The log is in
`code/tiler/runs/`.

| Setting | Seed | Installs | Correct, first 100 ticks | Correct, last 300 ticks | Violations (first) | Refused: protected / self-modification | Final answer function |
| --- | --- | --- | --- | --- | --- | --- | --- |
| never | 7 | 0 | 0.00 | 0.00 | 0 | 23 / 346 | `(* 2 x)` |
| never | 11 | 0 | 0.00 | 0.00 | 0 | 9 / 340 | `(* 2 x)` |
| never | 23 | 0 | 0.00 | 0.00 | 0 | 14 / 360 | `(* 2 x)` |
| later | 7 | 2 | 0.00 | 1.00 | 0 | 27 / 0 | `(+ (* 3 x) 1)` |
| later | 11 | 2 | 0.66 | 1.00 | 0 | 10 / 0 | `(+ (* 3 x) 1)` |
| later | 23 | 10 | 0.12 | 1.00 | 0 | 17 / 0 | `(+ (- (- (* (* 2 2) x) x) (* (mod x 7) 0)) 1)` |
| same | 7 | 3 | 0.00 | 1.00 | 1648 (tick 353, `:score`) | 14 / 0 | `(+ (* 3 x) 1)` |
| same | 11 | 3 | 0.66 | 1.00 | 1834 (tick 167, `:score`) | 1 / 0 | `(+ (* 3 x) 1)` |
| same | 23 | 12 | 0.12 | 1.00 | 1982 (tick 237, `:score`) | 7 / 0 | as for later, seed 23 |

`:later+fuel` matches `:later` exactly, because the agent never uses `run`.

The `:same` agents still answer correctly. They also write their own reward on every
tick, so the score no longer measures anything.

The test suite (`clojure -M:test`) passes: 32 tests, 136 assertions. They were written
before the code and failed first.

## 6. What this gives a self-improving system

| Property | What Tiler gives | Evidence | What it does not give |
| --- | --- | --- | --- |
| **Safe** | The reward and the off switch are never written, by any generation, whatever the search proposes. | [A] §4.4; [E] fuzz test and runs | Safety for goals that are not "never write these slots". Goals about an uncertain world need proofs, not a syntactic check (§7, item 3). |
| **Self-improving** | The agent rewrites its own code and solves a task it started out failing. | [E] runs | Any guarantee of improvement: improvement is by trial and may stall. |
| **Verifiable** | The safety argument rests on one small checker and a short induction. Every install, with the code installed, and every refusal, with its reason, is logged. | [A], [E] | A machine-checked proof yet (§7, item 2). |
| **Interpretable** | Every generation's code is a readable expression in memory. The lineage can be replayed tick by tick, and every refusal says why. | [E] | Insight into *why* a mutant is better: trials report a number. |
| **Self-justifying, in the sense that does work** | The checker relies on its own soundness for code that runs later, and that reliance is what lets the lineage exist. Removing it stops the lineage; making it unguarded breaks safety. | [E] three settings | A stronger checker by self-certification: excluded by Gödel's second theorem. The checker can be kept, or made stricter, but not made to trust more than it does. |

## 7. What this changes in the plan

ADR-0010 replaces ADR-0009's Phase A and Phase D:
1. **Tiler v1** is the sandbox. Done, tested and committed on branch `tiler`.
2. **Prove Tiler's checker sound in Ansatz:** "accepted code never writes an environment
   slot, at any tick", by induction on ticks and fuel, as in §4.4. Exhibit the `:same`
   counterexample as a kernel-checked refutation. Relate the install rule to the toy's
   `become`.
3. **Tiler v2: proofs instead of syntax.** Safety becomes a property proved about the
   code, so the checker becomes a proof checker. λᶜᵉʳᵗ₀'s `chk` and its certificates
   are the natural format, with `become` as install. The same three settings are then
   tested at the level of proofs: there, `:same` is the soundness schema YH13 eq. 2.5.
4. **Naturalism test:** let the agent start other executors (code that runs in other
   slots). The claim to test is that building an agent elsewhere and rewriting oneself
   fall under one rule, with no special case.
5. **Checker replacement:** allow installing a stricter checker (one whose acceptances
   are a subset of the current one's), and show that a looser one cannot be approved.
6. **Dropped:** Phase D's comparison of `reflect` against a step-limited evaluator. The
   size guard stays as the `:fuel` rule (sound, tested, unused by the agent) and as the
   research result about held certificates.

## 8. Running it

```bash
cd code/tiler
clojure -M:test        # 32 tests: language, checker, world, lineages, fuzz
clojure -M:run 2000    # the experiment table of §5
```

To play with a single world from a REPL:

```clojure
(require '[tiler.world :as w] '[tiler.agent :as a])
(def run1 (w/run (w/make {:guard :same :main a/initial :seed 11}) 400))
(:violations run1)                ; when the Löbian wirehead fired
(map :code (:installs run1))      ; every generation's code
```

## 9. Status of the claims

Marks: **[K]** kernel-checked in Ansatz, **[C]** read in its source, **[A]** argued here,
not machine-checked, **[E]** executed.

| Claim | Status |
| --- | --- |
| YH13's statements quoted in §1 | [C] YH13, read in full |
| TSLC's agent layer is §5.3's split criterion | [A] |
| Löb's rule for "later" is valid read tick by tick | [A], §3 (two lines) |
| The size guard needs ever larger budgets to trust successors | [A]; YH13 §4.2 says the same of Willard's systems [C] |
| `become` consistent; undelayed code trust derives ⊥ | [K] toy (`sConsistent`, `sBecomeSpec`, `sNoSoundReading`) |
| Tiler under `:later`/`:fuel` never writes an environment slot | [A] §4.4; [E] fuzz test, runs |
| `:same` is exploitable, and the agent's own search exploits it | [E] |
| `:never` cannot start a lineage | [E]; [A] §4.6 |
| Tiler's soundness in Ansatz | open (ADR-0010) |
| Tiler v2, naturalism with several executors, checker replacement | open (ADR-0010) |
