(ns lcert.test-runner
  "Runs the fast or the extended suite and exits non-zero on any failure.

  Usage (see bin/test-fast and bin/test-extended):
    clojure -M:test   -m lcert.test-runner fast [ns ...]
    clojure -M:ansatz -m lcert.test-runner extended [ns ...]

  With namespace arguments only those namespaces run, which is how a single
  red/green step is exercised.  The extended suite first loads the
  Ansatz-verified kernel and switches the runtime measures to it, so every
  language test is re-run on the verified functions."
  (:require [clojure.test :as t]))

(def fast-namespaces
  "The language's own tests, which need no Ansatz, and every example of
  TUTORIAL.md (lcert.tutorial-test), so the tutorial cannot drift."
  '[lcert.syntax-test
    lcert.encode-test
    lcert.reduce-test
    lcert.typing-test
    lcert.check-test
    lcert.eval-test
    lcert.core-test
    lcert.tutorial-test])

(def extended-namespaces
  "Tests of the Ansatz kernel itself, run before the fast suite is repeated,
  and the slower probe: the embedding of PA (step 1 of Proposition 5)."
  '[lcert.verified-test lcert.pa-test])

(defn -main [suite & only]
  (let [nss (cond
              (seq only)             (map symbol only)
              (= suite "fast")       fast-namespaces
              (= suite "extended")   (concat extended-namespaces fast-namespaces)
              :else (throw (ex-info "suite must be fast or extended" {:suite suite})))]
    (when (= suite "extended")
      ;; Loading the kernel runs the Ansatz elaborator and kernel on every
      ;; definition and theorem; a failure there aborts the suite.
      (require 'lcert.verified)
      ((requiring-resolve 'lcert.kernel/use-verified!)))
    (apply require nss)
    (let [{:keys [fail error]} (apply t/run-tests nss)]
      (shutdown-agents)
      (System/exit (if (zero? (+ fail error)) 0 1)))))
