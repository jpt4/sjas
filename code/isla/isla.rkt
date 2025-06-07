;; 20250531T21:22:30Z-0400EDT
;; isla.rkt
;; jpt4
;; Racket v8.14
;; IS-lambda(A) implementation

#lang racket

;; isla grammar
#|
TODO: Add equality and relations

exp := g-fun | q-exp
g-fun := number | monadic-g-function | dyadic-g-function
number :=
monadic-g-function := g-pred | g-log
dyadic-g-function := g-sub | g-div | g-max | g-root | g-count
|#

(define (isla-term? t)
  (match t
    [f #:when (g-fun? f) #t]
    [q #:when (q-exp? q) #t]
    [_ #f]
    ))

(define (g-fun? f)
  (match f
    [s #:when (symbol? s) #t]
    [n #:when (number? n) #t]
    [`(,g ,x) #:when (and (monadic-g-function? g) (g-fun? x)) #t]
    [`(,g ,x ,y) #:when (and (dyadic-g-function? g) (g-fun? x) (g-fun? y)) #t]
    [_ #f]
    ))

(define (monadic-g-function? f) (member f '(g-pred g-log)))

(define (dyadic-g-function? f) (member f '(g-sub g-div g-max g-root g-count)))

(define (q-exp? q)
  (match q
    [DELTA #:when (delta-zero? DELTA) #t]
    [SIGMA #:when (sigma-one? SIGMA) #t]
    [PI #:when (pi-one? PI) #t]
    [_ #f]
    ))

(define (delta-zero? e)
  (match e
    [`(FAB ,x ,t ,prop) #:when (and (symbol? x) 
                                    (number? t)
                                    (g-fun? prop)) #t]
    [`(TEB ,x ,t ,prop) #:when (and (symbol? x) 
                                    (number? t)
                                    (g-fun? prop)) #t]
    [`(FAB ,x ,t ,sen) #:when (and (symbol? x) 
                                    (number? t)
                                    (delta-zero? sen)) #t]
    [`(TEB ,x ,t ,sen) #:when (and (symbol? x) 
                                    (number? t)
                                    (delta-zero? sen)) #t]
    ))

(define (sigma-one? s)
  (match s
    [`(TE ,x ,e) #:when (and (symbol? x)
                             (delta-zero? e)) #t]
    [`(TE ,x ,e) #:when (and (symbol? x)
                             (sigma-one? e)) #t]
    ))

(define (pi-one? p)
  (match p
    [`(FA ,x ,e) #:when (and (symbol? x)
                             (delta-zero? e)) #t]
    [`(FA ,x ,e) #:when (and (symbol? x)
                             (pi-one? e)) #t]
    ))

;; isla-eval
(define (isla-eval e)
  ""
)

;; Grounding Functions
;substraction
(define (g-sub x y)
  (if (< x y)
      0
      (- x y)))
;division
(define (g-div x y)
  (if (= y 0)
      x
      (floor (/ x y))))
;predecessor
(define (g-pred x)
  (max (- x 1) 0))
;maximum
(define (g-max x y) (max x y))
;base 2 logarithm
(define (g-log x)
  (ceiling (log (+ x 1) 2)))
;nth root
(define (g-root x y)
  (cond
    [(= y 0) x]
    [(> y 1) (floor (expt x (/ 1 y)))]
    [else x]))
;count rightmost bits
;using list encoded, MSB bitstrings for x
(define (g-count x j)
  (count (lambda (a) (= a 1))
         (list-tail x (- (length x) j))))

