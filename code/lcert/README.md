# lcert — λᶜᵉʳᵗ₀, a minimal certificate language

A small, total, dependently typed functional language in which programs can
check proofs of their own typing and rely on the language's own consistency.
Proofs held as *certificates* are linear resources, paid for with tokens.

- **The design** is in [`../../nachlass/refinement/R4-certificate-calculus.md`](../../nachlass/refinement/R4-certificate-calculus.md).
- **The proofs** are in [`../../nachlass/refinement/R4-metatheory.md`](../../nachlass/refinement/R4-metatheory.md).
  This language implements its core calculus, λᶜᵉʳᵗ₀, rule for rule.
- **The charter** is ADR-0005:
  [`../../nachlass/refinement/ADR-0005-lcert-implementation.md`](../../nachlass/refinement/ADR-0005-lcert-implementation.md).

## The idea in one paragraph

A *code* (type `Syn`) is a free syntax tree: programs build and copy codes at
no cost. A *certificate* (type `R`) is the same tree, but every internal node
consumes one *token* (type `◇`). Tokens exist only as the program's budget:
`n` of them, named `$1 … $n`, each usable at most once. The built-in `chk′`
decides whether a code encodes a typing derivation of this very language. The
constant `H₁` says that no certificate proves a type while another proves its
negation. Because a certificate of `N` nodes costs `N` tokens, and must
declare fewer tokens than it has nodes, that axiom is consistent: the metatheory
proves it by induction on budgets. So programs may use it. No large numbers
are computed anywhere.

## Running

This needs the Clojure CLI. The fast suite needs nothing else. The extended
suite downloads Ansatz from Clojars on first use, and never a Mathlib store.

```
bin/test-fast        # the language: 41 tests
bin/test-extended    # adds the Ansatz-verified kernel, and re-runs everything on it
```

From a REPL (`clojure -M:test`):

```clojure
(require '[lcert.core :as lc])

;; the draft's example: not, its certificate has 35 nodes
(lc/certify 0 '(fn [x w Bool] (if x ff tt)))
;; => {:type [:Pi :w [:Bool] [:Bool]], :nodes 35, :budget 0, ...}

(lc/run 0 '((fn [x w Bool] (if x ff tt)) tt))        ;; => false

;; a certificate costs one token per node
(lc/run 2 '(node $1 :a (node $2 :b (leaf :c) (leaf :c)) (leaf :c)))

;; …and a token cannot be spent twice (a type error)
(lc/run 1 '(node $1 :a (node $1 :a (leaf :a) (leaf :a)) (leaf :a)))
```

## The language

The surface syntax is EDN. A binder `[x u A]` gives a variable, its *usage* —
`0` (erased), `1` (at most once), or `w` (unrestricted) — and its type.

| Types | |
| --- | --- |
| `Void` `Unit` `Bool` `Nat` `Lbl` `Syn` `Dia` `R` | empty, unit, Booleans, numbers, labels, codes, the token type `◇`, certificates |
| `(T b)` | the proposition that the Boolean `b` is true: `(T tt)` is `Unit`, `(T ff)` is `Void` |
| `(Pi [x u A] B)`, `(Sigma [x u A] B)` | dependent functions and pairs |
| `(-> A B)`, `(-o A B)` | functions taking their argument at `ω`, at `1` |
| `(tensor A B)`, `(prod A B)` | pairs whose first component is at `1`, at `ω` |

| Terms | |
| --- | --- |
| `(fn [x u A ...] body)`, `(f a b ...)` | functions and application |
| `star` `tt` `ff` `zero` `(succ n)` `3` `:label` | constants |
| `(if b t e)`, `(elim-bool [x P] b t e)` | case on a Boolean, non-dependent and dependent |
| `(rec-nat [x P] z [x y] s n)` | primitive recursion |
| `(case-lbl [x P] a {:label t ... :else t})` | case on a label |
| `(sleaf a)` `(snode a c1 c2)` `(rec-syn [x P] [a] t [a c1 c2 y1 y2] t c)` | codes |
| `(leaf a)` `(node d a r1 r2)` `(itr X g h r)` `(print r)` | certificates: `node` consumes the token `d`; `itr` iterates, handing each node's token to `h`; `print` forgets the tokens |
| `(pair S a b)`, `(let-pair C [x y] p t)`, `(abort A t)` | pairs, and absurdity |
| `(chk c d)` | does code `c` certify the closed type whose code is `d`? |
| `(H1 r s c e1 e2)` | no certificate `r` of a type and certificate `s` of its negation |
| `(reflect D r e)`, `(H r e)` | run a certified program of base data type `D` (`H` is `D = Void`) |
| `(inspect X r c [x e] t1 [x e] t2)` | check certificate `r` against `c`, and get it back |
| `(code A)`, `c-bot`, `(neg c)` | the code of a closed type; `⌜Void⌝`; the code of `A ⊸ Void` |

## How it is built

| Namespace | Role |
| --- | --- |
| `lcert.syntax` | abstract syntax, de Bruijn operations, label set, parser |
| `lcert.encode` | the encoding of derivations as codes (properties E1–E5), and decoding |
| `lcert.reduce` | conversion steps β, ι, δ, T; recorded normalization; chains |
| `lcert.typing` | the type checker; builds an explicit derivation for every judgment |
| `lcert.check` | `Check`, an independent structural validator of encoded derivations |
| `lcert.eval` | the erasing, budgeted evaluator, with distinct runtime tokens |
| `lcert.kernel` | node counts and budgets; switchable to the Ansatz-verified versions |
| `lcert.core` | the API |
| `lcert.examples` | executable versions of results in the metatheory |
| `ansatz/lcert/verified.clj` | the kernel in Ansatz: definitions and kernel-checked theorems |

Three design points:
- **Two checkers.** The type checker *builds* derivations, and `Check`
  *validates* them from the recorded judgments alone. The two share no code.
  The tests require that every derivation the first builds, the second accepts.
- **Erasure.** Programs run erased from their derivations: arguments and pair
  components at usage `0` never run. Without erasure, an erased position could
  build a certificate of any size from one token, which the typing rules allow
  there (review R4-04 in the metatheory's §8).
- **Recorded conversions.** Every conversion is kept as a chain of single steps,
  so `Check` never normalizes. The price is size. Certifying
  `(lit_not, ⋆) : □(Bool → Bool)` takes a 58,180-node certificate at budget 35,
  most of it the recorded computation.

## Test coverage

**The fast suite** covers:
- every rule of the metatheory's table, in both the type checker and `Check`;
- the draft's `not` example, exactly;
- the rejection of every resource violation;
- mutation of certificates;
- `reflect`, `inspect`, erasure;
- the metatheory's Propositions 4.9 (at depth 0) and 4.10, and its definable
  destructor.

**Not covered:**
- `H₁` at runtime — it is unreachable in typed programs, so nothing reaches
  it;
- the `abort` default — likewise unreachable;
- the recovery of Check from an internal exception — a malformed code is
  simply rejected.

**The extended suite** adds:
- the Ansatz kernel's theorems;
- agreement between the verified and plain measures, on every certificate in
  the corpus and on random codes.
