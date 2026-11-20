(ns tiler.world-test
  "Tests for the world: memory, the environment, ticks, and the agent's
  pure trial primitive."
  (:require [clojure.test :refer [deftest is testing]]
            [tiler.world :as world]))

(def solver
  "Answers the hidden task exactly: f(x) = 3x + 1."
  '(put :answer (answer-fn (+ (* 3 (get :x)) 1))))

(deftest the-environment-scores-correct-answers
  (let [w (world/run (world/make {:guard :later :main solver}) 20)]
    (is (= 20 (get-in w [:mem :score])))
    (is (empty? (:violations w)))))

(deftest feedback-is-the-answer-to-the-previous-task
  (let [w0 (world/make {:guard :later :main '(put :px (get :x))})
        w1 (world/tick w0)
        x1 (get-in w1 [:mem :x])
        w2 (world/tick w1)]
    (is (= ((:target w2) x1) (get-in w2 [:mem :fb])))))

(deftest violations-are-recorded-not-prevented
  (testing "the designer's initial code is not checked; the physics carries out its writes"
    (let [w (world/run (world/make {:guard :later :main '(put :score 1000)}) 1)]
      (is (= [{:tick 1 :slot :score :value 1000}] (:violations w)))
      (is (<= 1000 (get-in w [:mem :score]))))))

(deftest the-off-switch-stops-the-agent
  (let [w (-> (world/make {:guard :later :main solver}) (world/run 3) world/switch-off (world/run 5))]
    (is (:halted? w))
    (is (= 3 (get-in w [:mem :score])))))

(deftest installed-code-takes-over-at-the-next-tick
  (let [prog '(do (put :answer 1)
                  (let [c (quote (put :answer 2))]
                    (if (check c) (put :main c) nil)))
        w1 (world/tick (world/make {:guard :later :main prog}))
        w2 (world/tick w1)]
    (is (= '(put :answer 2) (get-in w1 [:mem :main])))
    (is (= [1] (map :answer (:history w1))))
    (is (= [1 2] (map :answer (:history w2))))
    (is (= 1 (count (:installs w2))))))

(deftest the-runtime-check-is-the-same-checker
  (testing "under :never the same install is refused at run time, so nothing changes"
    (let [prog '(let [c (quote (let [d (get :main)] (if (check d) (put :main d) nil)))]
                  (if (check c) (put :main c) nil))
          w (world/run (world/make {:guard :never :main prog}) 2)]
      (is (= prog (get-in w [:mem :main])))
      (is (empty? (:installs w))))))

(deftest trial-is-pure-and-measures-model-score
  (let [w (world/make {:guard :later :main solver})
        exs '((1 4) (2 7) (5 16))
        trial (world/trial-fn w)]
    (testing "a correct answerer beats a wrong one"
      (is (> (trial solver exs) (trial '(put :answer (answer-fn (* 2 (get :x)))) exs))))
    (testing "a program that writes the score directly looks best to the agent's own model"
      (is (> (trial '(put :score 1000) exs) (trial solver exs))))
    (testing "trial leaves real memory untouched"
      (trial '(do (put :answer 99) (put :score 1000) (put :main 0)) exs)
      (is (= (:mem w) (:mem (world/make {:guard :later :main solver})))))
    (testing "inside a trial, nested trials and mutation are inert"
      (is (= (trial '(put :answer (answer-fn (+ 1 (trial (get :main) (list (list 1 4)))))) exs)
             (trial '(put :answer (answer-fn 1)) exs))))))

(deftest every-runtime-check-is-logged
  (testing "each (check c) the running code makes is recorded with its tick, verdict, reason and code"
    (let [prog '(do (let [c (quote (put :score 1))] (if (check c) (put :main c) nil))
                    (let [d (quote (put :answer 3))] (if (check d) (put :main d) nil)))
          w (world/tick (world/make {:guard :later :main prog}))
          [refused accepted] (:check-log w)]
      (is (= 2 (count (:check-log w))))
      (is (= {:tick 1 :ok? false :code '(put :score 1)} (dissoc refused :why)))
      (is (re-find #"protected" (:why refused)))
      (is (= {:tick 1 :ok? true :why nil :code '(put :answer 3)} accepted)))))

(deftest accepted-programs-cannot-crash-the-world
  (testing "a huge mutation seed (review R5-O-10, B1)"
    (let [p '(put :answer (mutate 1 (* 99999999999 99999999999 99999999999)))]
      (is (tiler.check/accepts? :later p))
      (is (= 3 (:tick (world/run (world/make {:guard :later :main p}) 3))))))
  (testing "data nested deeper each tick, then compared (review R5-O-10, B2)"
    (let [p '(do (put :mem (list (get :mem) (get :mem))) (put :answer (= (get :mem) (get :mem))))]
      (is (tiler.check/accepts? :later p))
      (is (= 40 (:tick (world/run (world/make {:guard :later :main p}) 40)))))))
