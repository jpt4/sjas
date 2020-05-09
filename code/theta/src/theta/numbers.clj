;20200427Z
;jpt4
;miniKanren arithmetic in Clojure.

;;Because core.logic.arithmetic is non-relational, and CFD assumes the
;;size of the numeric domain in a way that Willard2017 does not, it is
;;necessary to re-implement the relational, reverse binary encoded
;;natural number system of The Reasoned Schemer, as written in
;;https://github.com/webyrd/faster-miniKanren/blob/master/numbers.scm

(ns theta.numbers
  (:require [clojure.core.logic :as lgc 
             :refer [!= == all appendo conde conso fresh lcons llist]
             :rename {== ==o}]             
            :reload-all))

;appendo : included in core.logic

;build-num : decimal natural numbers to reverse binary lists
(defn build-num [n]
      (cond
	(odd? n) 
	(cons 1 (build-num (/ (- n 1) 2)))
	(and (not (zero? n)) (even? n))
	(cons 0 (build-num (/ n 2)))
	(zero? n) '()))

;numo : define the grammar of natural numbers
(defn numo [x]
  (!= x '(0))
  (conde
   [(==o x '())]
   [(fresh [d] (==o (lcons 1 d) x) (numo d))]
   [(fresh [d] (==o (lcons 0 d) x) (!= '() d) (numo d))]))


(defn zeroo [n]
    (==o '() n))

(comment
Need (fresh) wrapping constraints in poso.
theta.core> (run 1 [q] (numo '()))
(_0)
theta.core> (run 1 [q] (numo '(0)))
()
theta.core> (run 1 [q] (poso q))
(())
theta.core> (run 1 [q] (poso '()))
(_0)
theta.core> (defn ietst [n] (!= '() n) (==o '() n))
#'theta.core/ietst
theta.core> (run 1 [q] (ietst q))
(())
theta.core> (doc !=)
-------------------------
clojure.core.logic/!=
([u v])
  Disequality constraint. Ensures that u and v will never
   unify. u and v can be complex terms.
nil
theta.core> (doc ==o)
-------------------------
clojure.core.logic/==
([u v])
  A goal that attempts to unify terms u and v.
nil
theta.core> )


(comment Order matters, numo before !=)
(defn poso [n]
  (all (numo n) (!= n '())))

(comment (defn poso
  [n]
    (fresh (a d)
      ;(conso a d n)
      (==o (lcons a d) n)
      ))
 )

(comment Order matters, poso before != to produce evens before odds.)
(defn >1o [n]
  (all (poso n) (!= '(1) n)))

(comment (defn >1o
  [n]
    (fresh (a ad dd)
      (==o (llist a ad dd) n)))
 )


(defn full-addero
  [b x y r c]
    (conde
      ((==o 0 b) (==o 0 x) (==o 0 y) (==o 0 r) (==o 0 c))
      ((==o 1 b) (==o 0 x) (==o 0 y) (==o 1 r) (==o 0 c))
      ((==o 0 b) (==o 1 x) (==o 0 y) (==o 1 r) (==o 0 c))
      ((==o 1 b) (==o 1 x) (==o 0 y) (==o 0 r) (==o 1 c))
      ((==o 0 b) (==o 0 x) (==o 1 y) (==o 1 r) (==o 0 c))
      ((==o 1 b) (==o 0 x) (==o 1 y) (==o 0 r) (==o 1 c))
      ((==o 0 b) (==o 1 x) (==o 1 y) (==o 0 r) (==o 1 c))
      ((==o 1 b) (==o 1 x) (==o 1 y) (==o 1 r) (==o 1 c))))

;declare gen-addero before addero
(declare gen-addero)

(defn addero
  [d n m r]
    (conde
      ((==o 0 d) (==o '() m) (==o n r))
      ((==o 0 d) (==o '() n) (==o m r)
       (poso m))
      ((==o 1 d) (==o '() m)
       (addero 0 n '(1) r))
      ((==o 1 d) (==o '() n) (poso m)
       (addero 0 '(1) m r))
      ((==o '(1) n) (==o '(1) m)
       (fresh (a c)
         ;(conso a c r)
         (==o (lcons a c) r)
         (full-addero d 1 1 a c)))
      ((==o '(1) n) (gen-addero d n m r))
      ((==o '(1) m) (>1o n) (>1o r)
       (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r))))

(defn gen-addero
  [d n m r]
    (fresh (a b c e x y z)
      ;(conso a x n)
      (==o (lcons a x) n)
      ;(conso b y m) (poso y)
      (==o (lcons b y) m) 
      ;(conso c z r) (poso z)
      (==o (lcons c z) r) 
      (full-addero d a b c e)
      (addero e x y z)))

;TOFIX duplicates, and (0), (0 0), etc.
(defn pluso
  [n m k]
    (addero 0 n m k))

;TOFIX improper lists synthesized as operands, e.g. (1 0 0 . 1)
(defn minuso
  [n m k]
    (pluso m k n))

;declare bound-*o odd-*o and *o
(declare bound-*o)

;declare odd-*o before *o
(declare odd-*o)

;TOFIX
;(run 10 [a b] (*o a b '()))
;([() _0] [(_0 . _1) ()])
(defn *o
  [n m p]
    (conde
      ((==o '() n) (==o '() p))
      ((poso n) (==o '() m) (==o '() p))
      ((==o '(1) n) (poso m) (==o m p))
      ((>1o n) (==o '(1) m) (==o n p))
      ((fresh (x z)
         (==o (lcons 0 x) n) (poso x)
         (==o (lcons 0 z) p) (poso z)
         (>1o m)
         (*o x m z)))
      ((fresh (x y)
         (==o (lcons 1 x) n) (poso x)
         (==o (lcons 0 y) m) (poso y)
         (*o m n p)))
      ((fresh (x y)
         (==o (lcons 1 x) n) (poso x)
         (==o (lcons 1 y) m) (poso y)
         (odd-*o x n m p)))))
;TOFIX
;(run 2 [a b c d] (odd-*o a b c d))
;([() _0 () (0)] [(_0 . _1) _2 () (0)])
(defn odd-*o
  [x n m p]
    (fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (pluso (lcons 0 q) m p)))

(comment
TOFIX
(run 2 [a b c d] (bound-*o a b c d))
([() (_0 . _1) _2 _3] [(_0) (_1 _2 . _3) () (_4 . _5)])
)
(defn bound-*o
  [q p n m]
    (conde
      ((==o '() q) (poso p))
      ((fresh (a0 a1 a2 a3 x y z)
         (==o (lcons a0 x) q)
         (==o (lcons a1 y) p)
         (conde
           ((==o '() n)
            (==o (lcons a2 z) m)
            (bound-*o x y z '()))
           ((==o (lcons a3 z) n) 
            (bound-*o x y z m)))))))

(comment
(run 4 [a b] (=lo a b))
([() ()] [(1) (1)] [(_0 1) (_1 1)] [(_0 _1 1) (_2 _3 1)])
)
(defn =lo
  [n m]
    (conde
      ((==o '() n) (==o '() m))
      ((==o '(1) n) (==o '(1) m))
      ((fresh (a x b y)
         (==o (lcons a x) n) (poso x)
         (==o (lcons b y) m) (poso y)
         (=lo x y)))))

(defn <lo
  [n m]
    (conde
      ((==o '() n) (poso m))
      ((==o '(1) n) (>1o m))
      ((fresh (a x b y)
         (==o (lcons a x) n) (poso x)
         (==o (lcons b y) m) (poso y)
         (<lo x y)))))

(defn <=lo
  [n m]
    (conde
      ((=lo n m))
      ((<lo n m))))

(comment
TOFIX producing equal as well as lesser values
(run 10 [a] (<o a '(1 1)))
(() (1) (1 1) (0 1) (0 1) (1 1) (0 1))
)
(defn <o
  [n m]
    (conde
      ((<lo n m))
      ((=lo n m)
       (fresh (x)
         (poso x)
         (pluso n x m)))))

(comment
TOFIX same as above
(run 10 [a] (<=o a '(1 1)))
(() (1) (1 1) (0 1) (0 1) (1 1) (0 1))
)
(defn <=o
  [n m]
    (conde
      ((==o n m))
      ((<o n m))))

;declare splito before divo
(declare splito)

;rename /o to divo
(comment
TOFIX duplicated results
(run 10 [a b c d] (divo '(1 0 1) '(1 1) c d))
([_0 _1 (1) (0 1)] [_0 _1 (1) (0 1)] [_0 _1 (1) (0 1)])
)
(defn divo
  [n m q r]
    (conde
      ((==o r n) (==o '() q) (<o n m))
      ((==o '(1) q) (=lo n m) (pluso r m n)
       (<o r m))
      ((<lo m n)
       (<o r m)
       (poso q)
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (splito n r nl nh)
         (splito q r ql qh)
         (conde
           ((==o '() nh)
            (==o '() qh)
            (minuso nl r qlm)
            (*o ql m qlm))
           ((poso nh)
            (*o ql m qlm)
            (pluso qlm r qlmr)
            (minuso qlmr nl rr)
            (splito rr r '() rh)
            (divo nh m qh rh)))))))

(comment
(run 4 [a b c d] (splito a b c d))
([() _0 () ()]
 [(0 _0 . _1) () () (_0 . _1)]
 [(1 . _0) () (1) _0]
 [(0 0 _0 . _1) (_2) () (_0 . _1)])
)
(defn splito
  [n r l h]
    (conde
      ((==o '() n) (==o '() h) (==o '() l))
      ((fresh (b n-ket)
         (==o (llist 0 b n-ket) n) ;`(0 ~b . ~n-ket) n)
         (==o '() r)
         (==o (lcons b n-ket) h) ;`(~b . ~n-ket) h)
         (==o '() l)))
      ((fresh (n-ket)
         (==o (lcons 1 n-ket) n) ;`(1 . ~n-ket) n)
         (==o '() r)
         (==o n-ket h)
         (==o '(1) l)))
      ((fresh (b n-ket a r-ket)
         (==o (llist 0 b n-ket) n) ;`(0 ~b . ~n-ket) n)
         (==o (lcons a r-ket) r) ;`(~a . ~r-ket) r)
         (==o '() l)
         (splito (lcons b n-ket) r-ket '() h))) ;`(~b . ~n-ket) r-ket '() h)))
      ((fresh (n-ket a r-ket)
         (==o (lcons 1 n-ket) n) ;`(1 . ~n-ket) n)
         (==o (lcons a r-ket) r) ;`(~a . ~r-ket) r)
         (==o '(1) l)
         (splito n-ket r-ket '() h)))
      ((fresh (b n-ket a r-ket l-ket)
         (==o (lcons b n-ket) n) ;`(~b . ~n-ket) n)
         (==o (lcons a r-ket) r) ;`(~a . ~r-ket) r)
         (==o (lcons b l-ket) l) ;`(~b . ~l-ket) l)
         (poso l-ket)
         (splito n-ket r-ket l-ket h)))))

;declare exp2 before logo
(declare exp2)

;declare repeated-mul before logo
(declare repeated-mul)

(comment
TOFIX duplicate values, trailing zeros
(run 10 [n b q r] (logo '(0 1 0 1) '(1 1) q r))
([_0 _1 (0 1) (1)]
 [_0 _1 (0 1) (1 0)]
 [_0 _1 (0 1) (1)]
 [_0 _1 (0 1) (1)]
 [_0 _1 (0 1) (1 0 0)]
 [_0 _1 (0 1) (1 0 0 0)]
 [_0 _1 (0 1) (1)]
 [_0 _1 (0 1) (1 0)]
 [_0 _1 (0 1) (1)]
 [_0 _1 (0 1) (1)])
TOFIX incorrect synthesized values
(run 8 [n b q r] (logo n '(1 1) q r))
([(1) _0 () ()]
 [(1) _0 () ()]
 [(1 1) _0 (1) ()]
 [(1 1) _0 () (0 1)]
 [(1 1) _0 () (0 1)]
 [(0 1) _0 () (1)]
 [(0 1) _0 () (1)]
 [(0 1) _0 () (1 0)])
)
(defn logo
  [n b q r]
    (conde
      ((==o '(1) n) (poso b) (==o '() q) (==o '() r))
      ((==o '() q) (<o n b) (pluso r '(1) n))
      ((==o '(1) q) (>1o b) (=lo n b) (pluso r b n))
      ((==o '(1) b) (poso q) (pluso r '(1) n))
      ((==o '() b) (poso q) (==o r n))
      ((==o '(0 1) b)
       (fresh (a ad dd)
         (poso dd)
         (==o (llist a ad dd) n) ;`(~a ~ad . ~dd) n)
         (exp2 n '() q)
         (fresh (s)
           (splito n dd r s))))
      ((fresh (a ad add ddd)
         (conde
           ((==o '(1 1) b))
           ((==o (llist a ad add ddd) b)))) ;`(~a ~ad ~add . ~ddd) b))))
       (<lo b n)
       (fresh (bw1 bw nw nw1 ql1 ql s)
         (exp2 b '() bw1)
         (pluso bw1 '(1) bw)
         (<lo q n)
         (fresh (q1 bwq1)
           (pluso q '(1) q1)
           (*o bw q1 bwq1)
           (<o nw1 bwq1))
         (exp2 n '() nw1)
         (pluso nw1 '(1) nw)
         (divo nw bw ql1 s)
         (pluso ql '(1) ql1)
         (<=lo ql q)
         (fresh (bql qh s qdh qd)
           (repeated-mul b ql bql)
           (divo nw bw1 qh s)
           (pluso ql qdh qh)
           (pluso ql qd q)
           (<=o qd qdh)
           (fresh (bqd bq1 bq)
             (repeated-mul b qd bqd)
             (*o bql bqd bq)
             (*o b bq bq1)
             (pluso bq r n)
             (<o n bq1)))))))

;unknown purpose
(defn exp2
  [n b q]
    (conde
      ((==o '(1) n) (==o '() q))
      ((>1o n) (==o '(1) q)
       (fresh (s)
         (splito n b s '(1))))
      ((fresh (q1 b2)
         (==o (lcons 0 q1) q) ;`(0 . ~q1) q)
         (poso q1)
         (<lo b n)
         (appendo b (lcons 1 b) b2) ;`(1 . ~b) b2)
         (exp2 n b2 q1)))
      ((fresh (q1 nh b2 s)
         (==o (lcons 1 q1) q) ;`(1 . ~q1) q)
         (poso q1)
         (poso nh)
         (splito n b s nh)
         (appendo b (lcons 1 b) b2)
         (exp2 nh b2 q1)))))

(comment
TOFIX synthesizing non-number values
(run 4 [n b q] (repeated-mul n b q))
([_0 (1) _0] [(_0 . _1) () (1)] [() (0 1) ()] [(1) (0 1) (1)])
TOFIX duplicate values
(run 4 [n q nq] (repeated-mul '(0 1) '(1 1) nq))
([_0 _1 (0 0 0 1)] [_0 _1 (0 0 0 1)])
)
(defn repeated-mul
  [n q nq]
    (conde
      ((poso n) (==o '() q) (==o '(1) nq))
      ((==o '(1) q) (==o n nq))
      ((>1o q)
       (fresh (q1 nq1)
         (pluso q1 '(1) q)
         (repeated-mul n q1 nq1)
         (*o nq1 n nq)))))

(comment
TOFIX invalid synthesized values
(run 4 [b n q] (expo b n q))
([(_0 . _1) () (1)]
 [() (_0 . _1) ()]
 [(1) (_0 . _1) (1)]
 [(_0 _1 . _2) () (1)])
)
(defn expo
  [b q n]
    (logo n b q '()))

