;20200427Z
;jpt4
;miniKanren arithmetic in Clojure.

;;Because core.logic.arithmetic is non-relational, and CFD assumes the
;;size of the numeric domain in a way that Willard2017 does not, it is
;;necessary to re-implement the relational, reverse binary encoded
;;natural number system of The Reasoned Schemer, as written in
;;https://github.com/webyrd/faster-miniKanren/blob/master/numbers.scm

(ns theta.numbers
  (:require [clojure.core.logic :as lgc]))

;appendo : included in core.logic

;build-num : decimal natural numbers to reverse binary lists
(defn build-num [n]
      (cond
	(odd? n) 
	(cons 1 (build-num (/ (- n 1) 2)))
	(and (not (zero? n)) (even? n))
	(cons 0 (build-num (/ n 2)))
	(zero? n) '()))

(defn zeroo
  [n]
    (lgc/== '() n))

(defn poso
  [n]
    (lgc/fresh (a d)
      (lgc/== `(,a . ,d) n)))

(defn >1o
  [n]
    (lgc/fresh (a ad dd)
      (lgc/== `(,a ,ad . ,dd) n)))

(defn full-addero
  [b x y r c]
    (lgc/conde
      ((lgc/== 0 b) (lgc/== 0 x) (lgc/== 0 y) (lgc/== 0 r) (lgc/== 0 c))
      ((lgc/== 1 b) (lgc/== 0 x) (lgc/== 0 y) (lgc/== 1 r) (lgc/== 0 c))
      ((lgc/== 0 b) (lgc/== 1 x) (lgc/== 0 y) (lgc/== 1 r) (lgc/== 0 c))
      ((lgc/== 1 b) (lgc/== 1 x) (lgc/== 0 y) (lgc/== 0 r) (lgc/== 1 c))
      ((lgc/== 0 b) (lgc/== 0 x) (lgc/== 1 y) (lgc/== 1 r) (lgc/== 0 c))
      ((lgc/== 1 b) (lgc/== 0 x) (lgc/== 1 y) (lgc/== 0 r) (lgc/== 1 c))
      ((lgc/== 0 b) (lgc/== 1 x) (lgc/== 1 y) (lgc/== 0 r) (lgc/== 1 c))
      ((lgc/== 1 b) (lgc/== 1 x) (lgc/== 1 y) (lgc/== 1 r) (lgc/== 1 c))))

(defn addero
  [d n m r]
    (lgc/conde
      ((lgc/== 0 d) (lgc/== '() m) (lgc/== n r))
      ((lgc/== 0 d) (lgc/== '() n) (lgc/== m r)
       (poso m))
      ((lgc/== 1 d) (lgc/== '() m)
       (addero 0 n '(1) r))
      ((lgc/== 1 d) (lgc/== '() n) (poso m)
       (addero 0 '(1) m r))
      ((lgc/== '(1) n) (lgc/== '(1) m)
       (lgc/fresh (a c)
         (lgc/== `(,a ,c) r)
         (full-addero d 1 1 a c)))
      File Edit Options Buffers Tools Sesman Clojure Help
      File Edit Options Buffers Tools Sesman REPL Help
      File Edit Options Buffers Tools Sesman Clojure CIDER CIDER Eval CIDER Interactions Help
                                        ;20200427Z                                                                                                                                                    |;; Connected to nREPL server - nrepl://localhost:35935
                                        ;jpt4                                                                                                                                                         |;; CIDER 0.25.0snapshot (package: 20200420.735), nREPL 0.7.0
                                        ;miniKanren arithmetic in Clojure.                                                                                                                            |;; Clojure 1.10.1, Java 1.8.0_161
      |;;     Docs: (doc function-name)
      ;;Because core.logic.arithmetic is non-relational, and CFD assumes the                                                                                        |;;           (find-doc part-of-name)
      ;;size of the numeric domain in a way that Willard2017 does not, it is                                                                                        |;;   Source: (source function-name)
      ;;necessary to re-implement the relational, reverse binary encoded                                                                                            |;;  Javadoc: (javadoc java-object-or-class)
      ;;natural number system of The Reasoned Schemer, as written in                                                                                                |;;     Exit: <C-c C-q>
      ;;https://github.com/webyrd/faster-miniKanren/blob/master/numbers.scm                                                                                         |;;  Results: Stored in vars *1, *2, *3, an exception in *e;
      |;; ======================================================================
      (ns theta.numbers                                                                                                                                             |;; If youâre new to CIDER it is highly recommended to go through its
          (:require [clojure.core.logic :as lgc]))                                                                                                                    |;; manual first. Type <M-x cider-view-manual> to view it.
      |;; In case youâre seeing any warnings you should consult the manualâs
                                        ;appendo : included in core.logic                                                                                                                             |;; "Troubleshooting" section.
      |;;
                                        ;build-num : decimal natural numbers to reverse binary lists                                                                                                  |;; Here are few tips to get you started:
      (defn build-num [n]                                                                                                                                           |;;
        (cond                                                                                                                                                   |;; * Press <C-h m> to see a list of the keybindings available (this
                                                                                                                                                                (odd? n)                                                                                                                                              |;;   will work in every Emacs buffer)
                                                                                                                                                                (cons 1 (build-num (/ (- n 1) 2)))                                                                                                                    |;; * Press <,> to quickly invoke some REPL command
                                                                                                                                                                (and (not (zero? n)) (even? n))                                                                                                                       |;; * Press <C-c C-z> to switch between the REPL and a Clojure file
                                                                                                                                                                (cons 0 (build-num (/ n 2)))                                                                                                                          |;; * Press <M-.> to jump to the source of something (e.g. a var, a
                                                                                                                                                                (zero? n) '()))                                                                                                                                       |;;   Java method)
      |;; * Press <C-c C-d C-d> to view the documentation for something (e.g.
      (defn zeroo                                                                                                                                                   |;;   a var, a Java method)
        [n]                                                                                                                                                         |;; * Enable âeldoc-modeâ to display function & method signatures in the minibuffer.
        (lgc/== '() n))                                                                                                                                           |;; * Print CIDERâs refcard and keep it close to your keyboard.
      |;;
      (defn poso                                                                                                                                                    |;; CIDER is super customizable - try <M-x customize-group cider> to
        [n]                                                                                                                                                         |;; get a feel for this. If youâre thirsty for knowledge you should try
        (lgc/fresh (a d)                                                                                                                                          |;; <M-x cider-drink-a-sip>.
          (lgc/== `(,a . ,d) n)))                                                                                                                                 |;;
      |;; If you think youâve encountered a bug (or have some suggestions for
      (defn >1o                                                                                                                                                     |;; improvements) use <M-x cider-report-bug> to report it.
        [n]                                                                                                                                                         |;;
        (lgc/fresh (a ad dd)                                                                                                                                      |;; Above all else - donât panic! In case of an emergency - procure
          (lgc/== `(,a ,ad . ,dd) n)))                                                                                                                            |;; some (hard) cider and enjoy it responsibly!
      |;;
      (defn full-addero                                                                                                                                             |;; You can remove this message with the <M-x cider-repl-clear-help-banner> command.
        [b x y r c]                                                                                                                                                 |;; You can disable it from appearing on start by setting
        (lgc/conde                                                                                                                                                |;; âcider-repl-display-help-bannerâ to nil.
                                                                                                                                                                  ((lgc/== 0 b) (lgc/== 0 x) (lgc/== 0 y) (lgc/== 0 r) (lgc/== 0 c))                                                                                      |;; ======================================================================
                                                                                                                                                                  ((lgc/== 1 b) (lgc/== 0 x) (lgc/== 0 y) (lgc/== 1 r) (lgc/== 0 c))                                                                                      |;;  Startup: /usr/bin/lein update-in :dependencies conj \[nrepl\ \"0.7.0\"\] -- update-in :plugins conj \[cider/cider-nrepl\ \"0.25.0-alpha1\"\] -- repl :head\
                                                                                                                                                                  ((lgc/== 0 b) (lgc/== 1 x) (lgc/== 0 y) (lgc/== 1 r) (lgc/== 0 c))                                                                                      |less :host localhost
                                                                                                                                                                  ((lgc/== 1 b) (lgc/== 1 x) (lgc/== 0 y) (lgc/== 0 r) (lgc/== 1 c))                                                                                      |theta.core> (load-file "src/theta/numbers.clj")
                                                                                                                                                                  ((lgc/== 0 b) (lgc/== 0 x) (lgc/== 1 y) (lgc/== 1 r) (lgc/== 0 c))                                                                                      |Syntax error compiling at (src/theta/numbers.clj:65:24).
                                                                                                                                                                        ((lgc/== 1 b) (((lgc/== '(1) n) (gen-addero d n m r))
      ((lgc/== '(1) m) (>1o n) (>1o r)
       (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r))))

(defn gen-addero
  [d n m r]
    (lgc/fresh (a b c e x y z)
      (lgc/== `(,a . ,x) n)
      (lgc/== `(,b . ,y) m) (poso y)
      (lgc/== `(,c . ,z) r) (poso z)
      (full-addero d a b c e)
      (addero e x y z)))

(defn pluso
  [n m k]
    (addero 0 n m k))

(defn minuso
  [n m k]
    (pluso m k n))

(defn *o
  [n m p]
    (lgc/conde
      ((lgc/== '() n) (lgc/== '() p))
      ((poso n) (lgc/== '() m) (lgc/== '() p))
      ((lgc/== '(1) n) (poso m) (lgc/== m p))
      ((>1o n) (lgc/== '(1) m) (lgc/== n p))
      ((lgc/fresh (x z)
         (lgc/== `(0 . ,x) n) (poso x)
         (lgc/== `(0 . ,z) p) (poso z)
         (>1o m)
         (*o x m z)))
      ((lgc/fresh (x y)
         (lgc/== `(1 . ,x) n) (poso x)
         (lgc/== `(0 . ,y) m) (poso y)
         (*o m n p)))
      ((lgc/fresh (x y)
         (lgc/== `(1 . ,x) n) (poso x)
         (lgc/== `(1 . ,y) m) (poso y)
         (odd-*o x n m p)))))

(defn odd-*o
  [x n m p]
    (lgc/fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (pluso `(0 . ,q) m p)))

(defn bound-*o
  [q p n m]
    (lgc/conde
      ((lgc/== '() q) (poso p))
      ((lgc/fresh (a0 a1 a2 a3 x y z)
         (lgc/== `(,a0 . ,x) q)
         (lgc/== `(,a1 . ,y) p)
         (lgc/conde
           ((lgc/== '() n)
            (lgc/== `(,a2 . ,z) m)
            (bound-*o x y z '()))
           ((lgc/== `(,a3 . ,z) n) 
            (bound-*o x y z m)))))))

(defn =lo
  [n m]
    (lgc/conde
      ((lgc/== '() n) (lgc/== '() m))
      ((lgc/== '(1) n) (lgc/== '(1) m))
      ((lgc/fresh (a x b y)
         (lgc/== `(,a . ,x) n) (poso x)
         (lgc/== `(,b . ,y) m) (poso y)
         (=lo x y)))))

(defn <lo
  [n m]
    (lgc/conde
      ((lgc/== '() n) (poso m))
      ((lgc/== '(1) n) (>1o m))
      ((lgc/fresh (a x b y)
         (lgc/== `(,a . ,x) n) (poso x)
         (lgc/== `(,b . ,y) m) (poso y)
         (<lo x y)))))

(defn <=lo
  [n m]
    (lgc/conde
      ((=lo n m))
      ((<lo n m))))

(defn <o
  [n m]
    (lgc/conde
      ((<lo n m))
      ((=lo n m)
       (lgc/fresh (x)
         (poso x)
         (pluso n x m)))))

(defn <=o
  [n m]
    (lgc/conde
      ((lgc/== n m))
      ((<o n m))))

(defn /o
  [n m q r]
    (lgc/conde
      ((lgc/== r n) (lgc/== '() q) (<o n m))
      ((lgc/== '(1) q) (=lo n m) (pluso r m n)
       (<o r m))
      ((<lo m n)
       (<o r m)
       (poso q)
       (lgc/fresh (nh nl qh ql qlm qlmr rr rh)
         (splito n r nl nh)
         (splito q r ql qh)
         (lgc/conde
           ((lgc/== '() nh)
            (lgc/== '() qh)
            (minuso nl r qlm)
            (*o ql m qlm))
           ((poso nh)
            (*o ql m qlm)
            (pluso qlm r qlmr)
            (minuso qlmr nl rr)
            (splito rr r '() rh)
            (/o nh m qh rh)))))))

(defn splito
  [n r l h]
    (lgc/conde
      ((lgc/== '() n) (lgc/== '() h) (lgc/== '() l))
      ((lgc/fresh (b n^)
         (lgc/== `(0 ,b . ,n^) n)
         (lgc/== '() r)
         (lgc/== `(,b . ,n^) h)
         (lgc/== '() l)))
      ((lgc/fresh (n^)
         (lgc/== `(1 . ,n^) n)
         (lgc/== '() r)
         (lgc/== n^ h)
         (lgc/== '(1) l)))
      ((lgc/fresh (b n^ a r^)
         (lgc/== `(0 ,b . ,n^) n)
         (lgc/== `(,a . ,r^) r)
         (lgc/== '() l)
         (splito `(,b . ,n^) r^ '() h)))
      ((lgc/fresh (n^ a r^)
         (lgc/== `(1 . ,n^) n)
         (lgc/== `(,a . ,r^) r)
         (lgc/== '(1) l)
         (splito n^ r^ '() h)))
      ((lgc/fresh (b n^ a r^ l^)
         (lgc/== `(,b . ,n^) n)
         (lgc/== `(,a . ,r^) r)
         (lgc/== `(,b . ,l^) l)
         (poso l^)
         (splito n^ r^ l^ h)))))

(defn logo
  [n b q r]
    (lgc/conde
      ((lgc/== '(1) n) (poso b) (lgc/== '() q) (lgc/== '() r))
      ((lgc/== '() q) (<o n b) (pluso r '(1) n))
      ((lgc/== '(1) q) (>1o b) (=lo n b) (pluso r b n))
      ((lgc/== '(1) b) (poso q) (pluso r '(1) n))
      ((lgc/== '() b) (poso q) (lgc/== r n))
      ((lgc/== '(0 1) b)
       (lgc/fresh (a ad dd)
         (poso dd)
         (lgc/== `(,a ,ad . ,dd) n)
         (exp2 n '() q)
         (lgc/fresh (s)
           (splito n dd r s))))
      ((lgc/fresh (a ad add ddd)
         (lgc/conde
           ((lgc/== '(1 1) b))
           ((lgc/== `(,a ,ad ,add . ,ddd) b))))
       (<lo b n)
       (lgc/fresh (bw1 bw nw nw1 ql1 ql s)
         (exp2 b '() bw1)
         (pluso bw1 '(1) bw)
         (<lo q n)
         (lgc/fresh (q1 bwq1)
           (pluso q '(1) q1)
           (*o bw q1 bwq1)
           (<o nw1 bwq1))
         (exp2 n '() nw1)
         (pluso nw1 '(1) nw)
         (/o nw bw ql1 s)
         (pluso ql '(1) ql1)
         (<=lo ql q)
         (lgc/fresh (bql qh s qdh qd)
           (repeated-mul b ql bql)
           (/o nw bw1 qh s)
           (pluso ql qdh qh)
           (pluso ql qd q)
           (<=o qd qdh)
           (lgc/fresh (bqd bq1 bq)
             (repeated-mul b qd bqd)
             (*o bql bqd bq)
             (*o b bq bq1)
             (pluso bq r n)
             (<o n bq1)))))))

(defn exp2
  [n b q]
    (lgc/conde
      ((lgc/== '(1) n) (lgc/== '() q))
      ((>1o n) (lgc/== '(1) q)
       (lgc/fresh (s)
         (splito n b s '(1))))
      ((lgc/fresh (q1 b2)
         (lgc/== `(0 . ,q1) q)
         (poso q1)
         (<lo b n)
         (appendo b `(1 . ,b) b2)
         (exp2 n b2 q1)))
      ((lgc/fresh (q1 nh b2 s)
         (lgc/== `(1 . ,q1) q)
         (poso q1)
         (poso nh)
         (splito n b s nh)
         (appendo b `(1 . ,b) b2)
         (exp2 nh b2 q1)))))

(defn repeated-mul
  [n q nq]
    (lgc/conde
      ((poso n) (lgc/== '() q) (lgc/== '(1) nq))
      ((lgc/== '(1) q) (lgc/== n nq))
      ((>1o q)
       (lgc/fresh (q1 nq1)
         (pluso q1 '(1) q)
         (repeated-mul n q1 nq1)
         (*o nq1 n nq)))))

(defn expo
  [b q n]
    (logo n b q '()))

