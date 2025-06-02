;; 20250531T21:22:30Z-0400EDT
;; isla.rkt
;; jpt4
;; Racket v8.14
;; IS-lambda(A) implementation

#lang racket

;; isla grammar

(define (isla-term t)
  (match t
    []
    )
  )

;; isla-eval
(define (isla-eval e)
  ""
)

;; Grounding Functions

(define (g-sub x y)
  (if (< x y)
      0
      (- x y)))

(define (g-div x y)
  (if (= y 0)
      x
      (floor (/ x y))))

(define (g-pred x)
  (max (- x 1) 0))

(define (g-maximum x y) (max x y))

(define (g-logarithm x)
  (ceiling (log (+ x 1) 2)))

(define (g-root x y)
  (cond
    [(= y 0) x]
    [(> y 1) (floor (expt x (/ 1 y)))]
    [else x]))

;using list encoded, MSB bitstrings for x
(define (g-count x j)
  (count (lambda (a) (= a 1))
         (list-tail x (- (length x) j))))

