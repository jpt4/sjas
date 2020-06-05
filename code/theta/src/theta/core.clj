;20200421Z
;jpt4
;Implement the languages described in Willard 2017

(ns theta.core
    (:require [clojure.core.logic :as lgc
               :refer :all :rename {== ==o}]
              [clojure.core.logic.fd :as fd]
              :reload-all))

;;Starting Notation Conventions

;Constant symbols

(def c0 0)
(def c1 1)
(def c2 2)

;Non-Growth functions

;Integer Subtraction
(defn isubo [x y z]
  (conde 
   [(fd/<= x y) (fd/== 0 z)]
   [(fd/< y x) (fd/+ z y x)]))

(defn idivo [x y z]
  (conde
   [(fd/== 0 y) (fd/== 0 z)]
   [(fd/== 0 x) (fd/== 0 z)]
   [(fd/< x y) (fd/== 0 z)]
   [(fresh [p d]
      (fd/>= x y)
      (fd/> y 0) (fd/* y z p) (fd/- x p d) (fd/< d y))]))

(defn maxo [x y m]
  (conde
   [(fd/>= x y) (fd/== x m)]
   [(fd/> y x) (fd/== y m)]))

(defn ilogo [l u b n r]
  (fresh [q res]
    (fd/in l u b n r q res (fd/interval l u))
    (conde
     [(fd/== 0 n) (fd/== 0 r)]
     [(fd/== 1 n) (fd/== 0 r)]
     [(fd/> n 1) 
      (idivo n b q)
      (ilogo l u b q res)
      (fd/+ 1 res r)])))

(comment
(run* [q] (ilogo-acc 0 100 q 9 0 2))
(3)
theta.core> (run* [q] (ilogo-acc 0 100 q 16 0 2))
(3 4)
theta.core> (run* [q] (ilogo-acc 0 100 3 16 0 q))
(2)
theta.core> (run* [q] (ilogo-acc 0 100 4 16 0 q))
(2)
theta.core> )

(defn ilogo-acc [l u b n acc r]
  (fresh [q res nacc]
    (fd/in l u b n acc r q nacc res (fd/interval l u))
    (conde
     [(fd/== 0 n) (isubo acc 1 r)]
     [(fd/== 1 n) (fd/== acc r)]
     [(fd/> n 1) 
      (idivo n b q)
      (fd/+ 1 acc nacc)
      (ilogo-acc l u b q nacc r)])))

(defn ilog2o [l u n r]
  (fd/in l u n r (fd/interval l u))
  (ilogo-acc l u 2 n 0 r))

(defn iexpo [l u b e r]
  (fresh [res s1]
    (fd/in l u b e r res (fd/interval l u))
    (conde
     [(fd/== 0 b) (fd/== 0 e) (fd/== 1 r)]
     [(fd/== 0 b) (fd/< 0 e) (fd/== 0 r)]
     [(fd/< 0 b) (fd/== 0 e) (fd/== 1 r)]
     [(fd/< 0 b) (fd/< 0 e)
      (fd/* b res r)
      (fd/- e 1 s1)
      (iexpo l u b s1 res)])))

(defn ilogo-w-iexpo [l u b n r]
  (fresh [e1 r1 e2 r2]
;    (log l u b n r e1 r1 e2 r2)
    (fd/in l u b n 
           ;r 
           e1 r e2 r2 (fd/interval l u))
    (iexpo l u b e1 r1) (iexpo l u b e2 r2)
    (fd/- e1 e2 1) (fd/!= r1 r2) (fd/> r1 n) (fd/>= n r2)
 ;   (log l u b n r e1 r1 e2 r2)
    (==o r e2)
  ;  (log l u b n r e1 r1 e2 r2)
     ))

(defn idivo-repeat [l u x b n r]
  (fresh [newx newn res]
    (fd/in l u x b n r newx newn res (fd/interval l u))
    (conde
     [(fd/== 0 x) 
      ;(fd/!= 0 n) 
      (fd/== 0 r)]
     [(fd/!= 0 x) (fd/== 0 n) (fd/== x r)]
     [(fd/!= 0 x) (fd/!= 0 n)
      (idivo x b newx)
      (fd/- n 1 newn)
;      (fd/+ 1 res r)
      (idivo-repeat l u newx b newn r)
      ])))

(comment
  repeatedly divide x by two fresh b's, up to n times, counting down
  n. if the quotient becomes zero, then b overshoots/is too large. If the quotient is 1 after exactly n divisions, then b is the root. If the quotient is greater than 1 after n divisions, then b undershoots, is too small. if the difference between b1 and b2 is 1, such that b1 overshoots and b2 undershoots or equals 1, then b2 is the n-root of x.)

(defn irooto [l u x y r]
  (fresh [x1 x2 r1 r2 b1 b2]
  (fd/in l u x y r x1 x2 r1 r2 (fd/interval l u))
  (conde
   [(fd/== 0 y) (fd/== 0 r)]
   [(fd/== 1 y) (fd/== x r)]
   [(fd/> y 1)
    (idivo-repeat l u x b1 y r1)
    (idivo-repeat l u x b2 y r2)
    (fd/- b1 b2 1) (fd/== 0 r1) (fd/>= r2 1)
    (fd/== b2 r)
    ])))

(defn eveno [n] (fresh [f] (fd/* f 2 n)))
(defn oddo [n] (fresh [f p] 
                  (fd/* f 2 p)
                  (fd/+ p 1 n)
                  ))

(defn build-numo [l u n r]
  (fresh [q s newn res]
    (fd/in l u n q s newn (fd/interval l u))
    (conde
     [(fd/== 0 n) (==o '(0) r)]
     [(fd/== 1 n) (==o '(1) r)]
     [(fd/> n 1)
      (oddo n) 
      (fd/- n 1 s)
      (fd/quot s 2 q)
      (build-numo l u q res)
      (conso 1 res r)]
     [(fd/> n 0) (eveno n)
      (fd/quot n 2 q)
      (build-numo l u q res)
      (conso 0 res r)]
)))

(defn counto-aux [l u b j acc r]
  (fresh [f d newacc resb]
    (fd/in l u j acc r f d newacc (fd/interval l u))
    (conde
     [(fd/== 0 j) (fd/== r acc)]
     [(fd/> j 0)
      (firsto b f) (resto b resb) 
      (fd/- j 1 d)
      (fd/== f 0) (counto-aux l u resb d acc r)]
     [(fd/> j 0)
      (firsto b f) (resto b resb) 
      (fd/- j 1 d)
      (fd/== f 1) (fd/+ acc 1 newacc)
      (counto-aux l u resb d newacc r)]
)))

(defn counto [l u i j r]
  (fresh [b]
    (fd/in l u i j (fd/interval 0 100))
    (build-numo l u i b)
    (counto-aux l u b j 0 r)))

(defn ipluso [l u x y z] 
  (fd/in l u x y z (fd/interval l u))
  (isubo z y x) (fd/>= z x))

(defn imulto [l u x y z]
  (fresh [s y2]
    (fd/in l u x y z s y2 (fd/interval l u))
    (conde
     [(fd/== 0 x) (fd/== 0 z)]
     [(fd/== 0 y) (fd/== 0 z)]
     [(fd/!= 0 x) (fd/!= 0 y)
      (isubo z 1 s)
      (idivo z x y)
      (idivo s x y2)
      (fd/< y2 y)])))
    
(defn ipredo [x p] (isubo x 1 p))

(defn ihalfo [x h] (idivo x 2 h))

(defn ipredno [x n p] (isubo x n p))

(defn ihalfno [x n p]
  (fresh [s res]
;    (fd/in l u x n p s res (fd/interval l u))
    (conde
     [(fd/== 0 n) (fd/== x p)]
     [(fd/> n 0)
      (ihalfo x res)
      (ipredo n s)
      (ihalfno res s p)
      ])))

(defn irooto-w-ilogo-w-iexpo [l u x y r]
  (fd/in l u x y r (fd/interval l u))
  (conde
   [(fd/== 0 y) (fd/== 0 r)]
   [(fd/>= y 1)
    (ilogo-w-iexpo l u r x y)]))

(comment
(run 1 [q] (irooto 0 100 8 3 q))
(2)
theta.core> (run 1 [q] (irooto 0 100 100 2 q))
(8)
theta.core> (run 2 [q] (irooto 0 100 100 2 q))
(8 9)
theta.core> (run 3 [q] (irooto 0 100 100 2 q))
(8 9 10)
theta.core> (run* [q] (irooto 0 100 100 2 q))
(8 9 10 5 6 7)
theta.core> 
)

(defn powero [l u x xout]
  (fresh [s l1 l2]
    (fd/in l u x s l1 l2 (fd/interval l u))
    (conde
     [(fd/== 1 x) (==o xout 't)]
     [(fd/!= 1 x)
      (isubo x 1 s) (ilog2o l u x l1) (ilog2o l u s l2)
      (fd/== l1 l2) (==o xout 'f)]
     [(fd/!= 1 x)
      (isubo x 1 s) (ilog2o l u x l1) (ilog2o l u s l2)
      (fd/!= l1 l2) (==o xout 't)]
     )))

(comment
;deprecated, thetao wraps thetacco
(defn theta [l u x xout]
  (fresh [y yout]
    (fd/in l u x xout y yout (fd/interval l u))
    (fd/!= xout 1)
    (conde
     [(powero l u x 't) (fd/!= x y)
      (powero l u xout 't)
      (theta l u y yout)
      (fd/!= xout yout)
      ]
     [(powero l u x 'f) (fd/== 0 xout)]
     )))
)


(defn lifted-membero [x l xout]
  (fresh [f r]
    (conde
     [(==o '() l) (==o 'f xout)]
     [(firsto l f) (==o l x) (==o 't xout)]
     [(firsto l f) (!= l x) (resto l r)
      (lifted-membero x r xout)])))      

(comment 
theta.core> (run 1 [q] (thetacc 0 100 1 '(1) q))
(2)
theta.core> (run 2 [q] (thetacc 0 100 1 '(1) q))
(2 2)
theta.core> (run 4 [q] (thetacc 0 100 1 '(1) q))
(2 2 4 2)
theta.core> (run 4 [q] (thetacc 0 100 2 '(1 2) q))
(4 4 8 4)
theta.core> (run 1 [q] (thetacc 0 100 4 '(1 2 4) q))
(8)
theta.core> (run 1 [q] 
              (fresh
                [a1 t1 a2 t2 t3]
                (fd/in a1 t1 a2 t2 t3 (fd/interval 0 100))
                (==o '(1) a1)
                (conso t1 a1 a2)
                (thetacc 0 100 1 a1 t1)
                (thetacc 0 100 t1 a2 q)))
              
(4)
)

(defn thetacco [l u x acc xout]
  (fresh [y yout nacc]
    (fd/in l u x xout y yout (fd/interval l u))
    (lifted-membero xout acc 'f)
    (conde
     [(powero l u x 't) (fd/!= x y)
      (powero l u xout 't)
      (conso xout acc nacc)
      (distincto nacc)
      (thetacco l u y nacc yout)
      (fd/!= xout yout)
      ]
     [(powero l u x 'f) (fd/== 0 xout)]
     )))      

(defn thetao [l u x xout] (thetacco l u x '(1) xout))

(defn thetaccno [l u x n acc xout]
  (fresh [a1 xnext anext nsub]
    (fd/in l u x n xout xnext nsub (fd/interval l u))
    (conde
     [(fd/== 1 n) (thetacco l u x acc xout)]
     [(fd/> n 1)      
      (==o acc a1)
      (thetacco l u x a1 xnext)
      (conso xnext a1 anext)
      (fd/- n 1 nsub)
      (thetaccno l u xnext nsub anext xout)
      ])))

(defn thetano [l u x n xout] (thetaccno l u x n '(1) xout))

(comment 
(defn thetacc [l u x acc xout]
  (fresh [y yout nacc]
    (fd/in l u x xout y yout (fd/interval l u))
    (lifted-membero xout acc 'f)
    (conde
     [(powero l u x 't) (fd/!= x y)
      (powero l u xout 't)
      (conso xout acc nacc)
      (thetacc l u y nacc yout)
      (fd/!= xout yout)
      ]
     [(powero l u x 'f) (fd/== 0 xout)]
     )))
)
