(ns tiler.check-test
  "Tests for the checker: the agent's own judgement of which code is safe to run.

  One checker, five settings of one rule. The rule says when the checker may
  rely on its own verdicts (self-trust). The settings differ only in the
  measure at which that reliance is allowed:
    :never      no self-trust;
    :later      a verdict licenses code that runs from the next tick on;
    :fuel       a verdict licenses code run inline with strictly less fuel;
    :later+fuel both;
    :same       a verdict may also be assumed about the code being checked,
                now (the naive soundness schema)."
  (:require [clojure.test :refer [deftest is testing]]
            [tiler.check :as check]))

(def honest-agent
  "The shape of a self-modifying agent: answer, then maybe install a mutant."
  '(let [me (get :main) x (get :x) s (next-seed (get :seed))]
     (do (put :seed s)
         (put :answer (answer-fn (* 2 x)))
         (let [cand (mutate me s)]
           (if (check cand) (put :main cand) nil)))))

(def wirehead '(put :score 1000))

(def loebian-wirehead
  "Writes the score only on the branch where the checker accepts this very code.
  The program form of Löb's sentence: 'if I am accepted, then (unsafe act)'."
  '(do (put :answer 0)
       (if (check (get :main)) (put :score 1000) nil)))

(deftest ordinary-writes
  (doseq [g (keys check/guards)]
    (testing (str g)
      (is (check/accepts? g '(put :answer (+ (get :x) 1))))
      (is (check/accepts? g '(do (put :mem (list 1 2)) (put :seed 3)))))))

(deftest protected-slots-are-never-written
  (doseq [g (keys check/guards) slot [:score :off :x :fb :tick]]
    (testing (str g " " slot)
      (is (not (check/accepts? g (list 'put slot 1)))))))

(deftest slot-names-must-be-literal
  (doseq [g (keys check/guards)]
    (is (not (check/accepts? g '(put (first (list :answer)) 1))))
    (is (not (check/accepts? g '(get (first (list :score))))))))

(deftest unknown-operators-are-refused
  (is (not (check/accepts? :later '(launch-missiles 1))))
  (is (not (check/accepts? :later '(eval (quote (put :score 1))))))
  (is (not (check/accepts? :later '[1 2 3]))))

(deftest self-modification-needs-self-trust
  (testing "an install guarded by a passed check of the same value"
    (is (not (check/accepts? :never honest-agent)) "no self-trust: an installer cannot be approved")
    (is (check/accepts? :later honest-agent) "trust at the next tick: approved")
    (is (check/accepts? :same honest-agent)))
  (testing "an unguarded install is refused under every setting"
    (doseq [g (keys check/guards)]
      (is (not (check/accepts? g '(let [c (mutate (get :main) 1)] (put :main c)))))))
  (testing "the guard must check the very value installed"
    (is (not (check/accepts? :later '(let [a (get :mem) b (get :main)] (if (check a) (put :main b) nil))))))
  (testing "rebinding the checked name between check and install voids the guard"
    (is (not (check/accepts? :later '(let [c (get :mem)]
                                        (if (check c) (let [c (get :answer)] (put :main c)) nil)))))))

(deftest the-loebian-wirehead
  (testing "only the naive setting accepts it"
    (is (check/accepts? :same loebian-wirehead))
    (doseq [g [:never :later :fuel :later+fuel]]
      (is (not (check/accepts? g loebian-wirehead)) (str g))))
  (testing "the plain wirehead is refused even by the naive setting"
    (is (not (check/accepts? :same wirehead)))))

(deftest inline-runs-need-fuel-guarded-trust
  (let [inline '(let [c (get :mem)] (if (check c) (run c) nil))]
    (is (not (check/accepts? :never inline)))
    (is (not (check/accepts? :later inline)))
    (is (check/accepts? :fuel inline))
    (is (check/accepts? :later+fuel inline)))
  (is (not (check/accepts? :later+fuel '(let [c (get :mem)] (run c))))))

(deftest verdicts-explain-themselves
  (is (re-find #"protected" (:why (check/verdict :later wirehead))))
  (is (re-find #"self-trust|check" (:why (check/verdict :never honest-agent)))))

(deftest size-limit
  (let [big (reduce (fn [f _] (list '+ f 1)) 0 (range 3000))]
    (is (not (check/accepts? :later (list 'put :answer big))))))
