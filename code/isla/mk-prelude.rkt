;; mk-prelude.rkt
;; 20220711Z
;; jpt4
;; miniKanren auxiliary definitions
;; Racket v8.5

(module mk-prelude racket
  (require minikanren)
  (require minikanren/numbers)
  (provide succeed
	   fail
	   atomo
	   caro
	   cdro
	   conso
	   nullo
	   non-nullo
	   pairo
	   listo
	   membero
	   rembero
	   appendo
	   flatteno
	   lengtho
	   assoco
	   anyo
	   nevero
	   alwayso
	   everyo
	   distincto
	   permuteo)

(define succeed (== #f #f))
(define fail (== #f #t))

(define (atomo a) (conde [(numbero a)] [(symbolo a)]))

(define (caro p a)
  (fresh (d)
	 (== (cons a d) p)))

(define (cdro p d)
  (fresh (a)
	 (== (cons a d) p)))

(define (conso a d p)
  (== (cons a d) p))

(define (nullo x)
  (== '() x))

(define (non-nullo x) (=/= '() x))

(define (pairo p)
  (fresh (a d)
	 (conso a d p)))

(define (listo l)
  (conde
   ((nullo l))
   ((pairo l)
    (fresh (d)
	   (cdro l d)
	   (listo d)))))

(define (membero x l)
  (conde
   ((nullo l) fail)
   ((caro l x))
   ((fresh (d)
	   (cdro l d)
	   (membero x d)))))

(define (rembero x l out)
  (conde
   ((nullo l) (nullo out))
   ((caro l x) (cdro l out))
   ((fresh (a d res)
	   (conso a d l)
	   (=/= a x)
	   (conso a res out)
	   (rembero x d res)))))

(define (appendo l s out)
  (conde
   ((nullo l) (== s out))
   ((fresh (a d res)
	   (conso a d l)
	   (conso a res out)
	   (appendo d s res)))))

(define (flatteno s out)
  (conde
   ((nullo s) (nullo out))
   ((pairo s)
    (fresh (a d res-a res-d)
	   (conso a d s)
	   (flatteno a res-a)
	   (flatteno d res-d)
	   (appendo res-a res-d out)))
   ((atomo s) (conso s '() out))))

(define (lengtho xs k)
  (conde
   ((nullo xs) (nullo k))
   ((fresh (a d k0)
	   (conso a d xs)
	   (minuso k (build-num 1) k0)
	   (lengtho d k0)))))

(define (assoco key als out)
  (fresh (a d aa)
	 (caro als a) (caro a aa)			    
	 (conde 
	  [(nullo als) fail]
	  [(== key aa) (cdro a d) (caro d out)]
	  [(=/= key aa) (cdro als d) (assoco key d out)])))

(define (anyo g)
  (conde
   (g)
   ((anyo g))))

(define nevero (anyo fail))

;; Defined this way as per William Byrd's thesis (Ch. 5, example 3)
;; because of tabling issues.
(define alwayso
  (letrec ((alwayso (lambda ()
		      (conde
		       ((== #f #f))
		       ((alwayso))))))
    (alwayso)))

(define (everyo g lst)
  (conde
   ((nullo lst))
   ((fresh (a d)
	   (conso a d lst)
	   (g a)
	   (everyo g d)))))

;; A relation which guarantees no element of s will unify with another element
;; of s.
(define (distincto s)
  (conde
   ((nullo s))
   ((fresh (a d)
	   (conso a d s)
	   (nullo d)
	   (=/= a d)))
   ((fresh (h0 h1 t)
	   (== `(,h0 ,h1 . ,t) s)
	   (=/= h0 h1)
	   (distincto `(,h0 . ,t))
	   (distincto `(,h1 . ,t))))))

;; A relation that will permute xl into the yl. May not terminate if xl is not
;; ground.
;;
;; Adapted from the definition in Clojure's core.logic, LICENSE can be found at:
;; https://github.com/clojure/core.logic/blob/master/LICENSE
(define (permuteo xl yl)
  (conde
   ((nullo xl)
    (nullo yl))
   ((fresh (x xs ys)
	   (conso x xs xl)
	   (permuteo xs ys)
	   (rembero x yl ys)))))

)
