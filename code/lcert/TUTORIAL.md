# Writing programs in λᶜᵉʳᵗ₀

A short tutorial. Every example below runs as shown: the test suite
(`bin/test-fast`) runs this file, and checks each result against the `;; =>`
line under it.

λᶜᵉʳᵗ₀ is a total functional language in which:
- programs are proofs;
- a built-in checker decides whether a piece of syntax is a proof in the
  language itself;
- *certificates* of such proofs cost tokens, and cannot be copied.

This tutorial is about using it. For what it is, see
[`README.md`](README.md). For why it is consistent, see the metatheory,
[`R4-metatheory.md`](../../nachlass/refinement/R4-metatheory.md).

## 0. Setup

From `code/lcert`, start a REPL with `clojure -M:test`, then:

```clojure
(require '[lcert.core :as lc] '[lcert.pa :as pa] '[lcert.examples :as ex])
```

- **Programs are EDN data**, quoted. `(lc/run n program)` type-checks a
  program, then runs it.
- **`n` is its budget:** the number of tokens it is given (§5). Most programs
  need none.
- **Clojure is the host language.** You assemble larger programs from pieces
  with ordinary Clojure (§2).

## 1. Values and functions

```clojure
(lc/run 0 '(succ (succ zero)))
;; => 2
(lc/run 0 3)
;; => 3
(lc/run 0 '(pair (prod Bool Nat) tt 7))
;; => [:pv true 7]
(lc/run 0 '((fn [x w Bool] (if x ff tt)) tt))
;; => false
```

**A binder is a triple `[name usage type]`.** The usage says how often the
variable may be used when the program runs:
- `w` — any number of times;
- `1` — at most once;
- `0` — never at run time, only inside types.

The type checker enforces this, so a usage-1 variable used twice is rejected:

```clojure
(lc/run 0 '(fn [x 1 Bool] (if x x ff)))
;; => :type-error
```

Types are written `Bool`, `Nat`, `Unit`, `Void`, `(-> A B)` (a function taking
its argument at `w`), `(-o A B)` (at `1`), `(Pi [x u A] B)`, `(prod A B)`,
`(tensor A B)`, `(Sigma [x u A] B)`, and `(T b)` (§3).

## 2. Definitions and recursion

`let` names a value: `(let [name usage type value] body)`, like a binder of
`fn` followed by its value. `rec-nat` is recursion on a number:
`(rec-nat [x P] base [k y] step n)`.
- `P` is the result type.
- In the step, `k` is the predecessor and `y` the result for it.

```clojure
(lc/run 0 '(let [double w (-> Nat Nat) (fn [n w Nat] (rec-nat [q Nat] zero [k y] (succ (succ y)) n))]
             (double 21)))
;; => 42
```

**Build larger programs in Clojure.** For instance, a prelude of arithmetic,
bound by `let` around any program body:

```clojure
(def arith
  '[plus  w (-o Nat (-> Nat Nat)) (fn [a 1 Nat b w Nat] (rec-nat [q Nat] a [k y] (succ y) b))
    times w (-> Nat (-o Nat Nat)) (fn [a w Nat b 1 Nat] (rec-nat [q Nat] zero [k y] (plus y a) b))])

(defn with-arith [body] (list 'let arith body))

(lc/run 0 (with-arith '(times 6 7)))
;; => 42
(lc/run 0 (with-arith '(rec-nat [q Nat] 1 [k y] (times (succ k) y) 5)))
;; => 120
```

**The one rule that surprises newcomers.** The recursive result `y` is
available *once*. You may pass it only to a function that takes that argument
at usage `1`. That is why `plus` takes its first argument at `1`, and `times`
its second. With an unrestricted `times2`, factorial is rejected:

```clojure
(lc/run 0 '(let [times2 w (-> Nat Nat Nat)
                 (fn [a w Nat b w Nat]
                   (rec-nat [q Nat] zero [k y] (rec-nat [q Nat] y [k2 y2] (succ y2) a) b))]
             (rec-nat [q Nat] 1 [k y] (times2 (succ k) y) 5)))
;; => :type-error
```

## 3. Proofs are programs

`(T b)` is the proposition "`b` computes to `tt`": `(T tt)` is `Unit`, and
`(T ff)` is `Void`. `lcert.pa` provides arithmetic for writing propositions:
- `EQ`, `PLUS` and `TIMES`;
- the lemmas `REFL`, which gives `x = x`, and `TRANSPORT`.

`pa/expand` inlines them. A true equation is proved by `star`, because the
type checker computes:

```clojure
(lc/run 0 (pa/expand '(the (T (EQ (PLUS 2 2) 4)) star)))
;; => :star
(lc/run 0 (pa/expand '(the (T (EQ (PLUS 2 2) 5)) star)))
;; => :type-error
```

**A universal statement is a function, and induction is recursion.** Here is
`∀x. 0 + x = x`:
- the base case is `star`;
- the step turns a proof `y` for `k` into one for `succ k`. The checker sees
  that the two types are the same.

```clojure
(def zero-plus
  (pa/expand '(fn [x w Nat] (rec-nat [x (T (EQ (PLUS zero x) x))] star [k y] y x))))

(some? (lc/check-program 0 zero-plus))
;; => true
(lc/run 0 (list zero-plus 3))
;; => :star
(lc/check-program 0 (pa/expand '(fn [x w Nat] (rec-nat [x (T (EQ (PLUS zero x) (succ x)))] star [k y] y x))))
;; => :type-error
```

**A false equation proves anything:**

```clojure
(some? (lc/check-program 0 (pa/expand '(fn [e w (T (EQ zero (succ zero)))] (abort Nat e)))))
;; => true
```

## 4. Programs that inspect programs

**Codes are trees.** A value of type `Syn`, a *code*, is a finite binary tree
whose nodes carry labels. There are two constructors:
- `(sleaf l)`, a leaf with label `l`, printed `[:sl l]`;
- `(snode l left right)`, an internal node, printed `[:sn l left right]`.

Labels are keywords from a fixed set, which includes `:a`, `:b` and `:c`.
Codes cost nothing: build them, copy them, pass them around freely.

```clojure
(lc/run 0 '(snode :a (sleaf :b) (sleaf :c)))
;; => [:sn :a [:sl :b] [:sl :c]]
```

**Recursion over a code** is `(rec-syn [x P] [a] leaf-case [a c1 c2 y1 y2] node-case c)`:
- `P` is the result type;
- in `leaf-case`, `a` is the leaf's label;
- in `node-case`, `a` is the node's label, `c1` and `c2` its children, and
  `y1` and `y2` the results already computed for them.

This counts the internal nodes of a code. As in §2, the recursive results `y1`
and `y2` may each be used once, so `add` takes both arguments at usage `1`:

```clojure
(lc/run 0 '(let [add w (-o Nat (-o Nat Nat)) (fn [a 1 Nat b 1 Nat] (rec-nat [q Nat] a [k y] (succ y) b))]
             (rec-syn [x Nat] [a] zero [a c1 c2 y1 y2] (succ (add y1 y2))
                      (snode :a (snode :b (sleaf :c) (sleaf :c)) (sleaf :c)))))
;; => 2
```

**Programs have codes too.** When the type checker accepts a program, it
builds a *derivation*: a tree recording each typing rule it used. That tree,
written as a code, is what `lc/certify` returns. The root is labelled with the
last rule used, and its first child records what was proved: the program and
its type.

```clojure
(def not-cert (:code (lc/certify 0 '(fn [x w Bool] (if x ff tt)))))

(take 2 not-cert)
;; => (:sn :Lam)
(nth not-cert 2)
;; => [:sn :has [:sl :empty] [:sn :pair [:sn :lam [:sl :bool] [:sn :if [:sn :var [:sl :zero] [:sl :nil]] [:sn :branches [:sl :ff] [:sl :tt]]]] [:sn :arrow [:sl :bool] [:sl :bool]]]]
```

Read the second result as "in the empty context, `λx. if x then ff else tt`
has type `Bool → Bool`". Variables are numbered, so `x` is `[:sn :var [:sl :zero] …]`.

Types have codes as well. `(code A)` is the code of the type `A` written in
the program:

```clojure
(lc/run 0 '(code (-> Bool Bool)))
;; => [:sn :arrow [:sl :bool] [:sl :bool]]
```

**`chk` checks a code.** `(chk c d)` is `tt` exactly when `c` is a valid
derivation, and the type it concludes with has code `d`. It is an ordinary
function from two codes to `Bool`, and costs nothing.

To pass a code computed in Clojure into a program, use `code-literal`. It
turns a code value, such as `not-cert`, into program text. `(code A)` differs:
it takes a *type* written in the program.

```clojure
(lc/run 0 (list 'chk (list 'code-literal not-cert) '(code (-> Bool Bool))))
;; => true
(lc/run 0 (list 'chk (list 'code-literal not-cert) '(code (-> Nat Nat))))
;; => false
```

## 5. Tokens and certificates

**Tokens are not made inside the language.** The type `Dia`, which is `◇`,
has no constructor, and nothing produces one. A program's tokens are its
*budget*:
- whoever runs it supplies them: `(lc/run n …)` hands over `n` fresh tokens,
  named `$1 … $n`;
- inside, a token can only be spent, passed on, recovered, or dropped.

A *certificate*, type `R`, is a code whose every internal node spends a token:

```clojure
(lc/run 2 '(print (node $1 :a (leaf :b) (node $2 :c (leaf :b) (leaf :b)))))
;; => [:sn :a [:sl :b] [:sn :c [:sl :b] [:sl :b]]]
(lc/run 1 '(node $1 :a (leaf :b) (node $1 :c (leaf :b) (leaf :b))))
;; => :type-error
(lc/run 0 '(node star :a (leaf :b) (leaf :b)))
;; => :type-error
```

`print` turns a certificate into its code.

**Tokens can be recycled, never multiplied.** `itr` iterates over a
certificate and hands each node's token to the node function, which may spend
it again. Relabelling a certificate costs nothing extra:

```clojure
(lc/run 2 '(print (itr R (fn [l w Lbl] (leaf :c))
                       (fn [d 1 Dia] (fn [l w Lbl] (fn [u 1 R] (fn [v 1 R] (node d :a u v)))))
                       (node $1 :a (leaf :b) (node $2 :c (leaf :b) (leaf :b))))))
;; => [:sn :a [:sl :c] [:sn :a [:sl :c] [:sl :c]]]
```

A certificate cannot be copied, any more than a banknote:

```clojure
(lc/check-program 0 '(fn [r 1 R] (pair (tensor R R) r r)))
;; => :type-error
```

**So a program holds, at any moment, no more certificate nodes than tokens it
was given.** That is the property the consistency proof rests on.

## 6. Certificates of programs: pay, check, run

The certificate of `not`'s derivation costs 35 tokens. `lc/certificate-form`
writes the program that builds it:

```clojure
(:nodes (lc/certify 0 '(fn [x w Bool] (if x ff tt))))
;; => 35
(= not-cert (lc/run 35 (list 'print (lc/certificate-form not-cert))))
;; => true
(lc/run 34 (list 'print (lc/certificate-form not-cert)))
;; => :error
```

With 34 tokens the program names a token, `$35`, that it was never given, and
is rejected before it runs.

**Certified evaluation.** `(reflect D r e)` runs the program that certificate
`r` certifies. It needs evidence `e` that `r` checks at type `D`. The run uses
fewer of `r`'s tokens than `r` holds:

```clojure
(def two (lc/certify 0 '(succ (succ zero))))
(:nodes two)
;; => 14
(lc/run 14 (list 'reflect 'Nat (lc/certificate-form (:code two)) 'star))
;; => 2
```

Here `star` is accepted as the evidence because the checker computes that the
certificate checks.

**Inspecting a certificate received at run time.** `inspect` checks it, gives
it back, and branches:

```clojure
(lc/run 2 '(inspect Bool (node $1 :a (leaf :b) (node $2 :c (leaf :b) (leaf :b))) c-bot
                    [x e] tt
                    [x e] ff))
;; => false
```

## 7. Relying on the language's consistency

**`H` says that no certificate checks at type `Void`:** no paid-for proof
proves falsity. So when `inspect` asks whether a certificate checks at `c-bot`,
the code of `Void`, a program may treat the yes-branch as impossible, and put
anything there:

```clojure
(def guard '(fn [r 1 R] (inspect Nat r c-bot [x e] (abort Nat (H x e)) [x e] 7)))

(some? (lc/check-program 0 guard))
;; => true
(lc/run 1 (list guard '(node $1 :a (leaf :b) (leaf :b))))
;; => 7
```

**`H₁` says the same of a certificate of a type paired with a certificate of
its negation.** Its type is itself a closed program:

```clojure
(some? (lc/check-program 0 '(fn [r 1 R s 1 R c w Syn e1 1 (T (chk (print r) c)) e2 1 (T (chk (print s) (neg c)))]
                              (H1 r s c e1 e2))))
;; => true
```

**What the language cannot prove** is the same statement about free codes,
`Π(c :ω Syn). T(chk c c-bot) → Void`. That is Gödel's second theorem, which
the metatheory proves for this calculus (its §6). Consistency is available
only for certificates, which are paid for.

## 8. Turning a code into a certificate, inside a program

A code and a certificate can describe the same tree. The certificate also
holds a token at each internal node. Take this code, with two internal nodes:

```clojure
(def small '[:sn :b [:sl :a] [:sn :c [:sl :a] [:sl :a]]])

(lc/certificate-form small)
;; => (node $1 :b (leaf :a) (node $2 :c (leaf :a) (leaf :a)))
```

That is the certificate written out by hand, which is how §6 built one. A
program can instead do the conversion itself, on a code it only receives at
run time. It cannot make tokens (§5), so it must be handed a *supply*. The
supply is a certificate whose tokens the parser takes one at a time, down its
right-hand side, one per internal node of the code.

`ex/parse-then` wraps the parser of `lcert.examples`. It parses a code with a
supply, binds the certificate to `t` and the unused supply to `rest`, and
evaluates a body. Here the supply has three tokens; the code needs two:

```clojure
(def supply '(node $1 :a (leaf :a) (node $2 :a (leaf :a) (node $3 :a (leaf :a) (leaf :a)))))

(lc/run 3 (ex/parse-then (list 'code-literal small) supply 'Syn '(print t)))
;; => [:sn :b [:sl :a] [:sn :c [:sl :a] [:sl :a]]]
```

Printing the certificate gives the code back. With one token, the parser runs
out, and the tree comes out truncated:

```clojure
(lc/run 1 (ex/parse-then (list 'code-literal small) '(node $1 :a (leaf :a) (leaf :a)) 'Syn '(print t)))
;; => [:sn :b [:sl :a] [:sl :c]]
```

**The whole pipeline.** A program receives the code of another program's
derivation. It pays for a certificate from its supply, checks that the
certificate proves a `Nat`, and runs it with `reflect`:

```clojure
(def two (lc/certify 0 '(succ (succ zero))))
(:nodes two)
;; => 14

(def supply14
  (reduce (fn [acc i] (list 'node (symbol (str "$" i)) :a '(leaf :a) acc))
          '(leaf :a) (range 14 0 -1)))

(lc/run 14 (ex/parse-then (list 'code-literal (:code two)) supply14 'Nat
                          '(inspect Nat t (code Nat) [x e] (reflect Nat x e) [x e] zero)))
;; => 2
```

The derivation code arrives as plain data. It becomes runnable only after 14
tokens are spent on it, and only after `inspect` confirms that it checks.

## 9. Reading errors

Type errors are Clojure exceptions whose data has `:type :lcert/type-error`,
and whose message says what went wrong. Variables are counted from the
innermost: index `0` is the last one bound.

```clojure
(try (lc/run 1 '(node $1 :a (leaf :b) (node $1 :c (leaf :b) (leaf :b))))
     (catch clojure.lang.ExceptionInfo e (.getMessage e)))
;; => "variable used beyond its declared usage in a certificate node: index 0 declared 1, used :w"
```

## Where next

- `lcert.examples` holds larger programs:
  - `H°` from `H₁`;
  - the certificate destructor;
  - the parser;
  - bounded code consistency.
- `lcert.pa` translates any proof of Peano arithmetic into this language.
