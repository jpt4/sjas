;; The formal suite's runner (bin/test-formal). A script file rather than
;; `clojure -e`: after -e, clojure.main reads the first remaining argument as
;; a script path, so the first suite named on the command line was silently
;; dropped.
(require 'clojure.test 'lcert.formal-test)
(def nss (if (seq *command-line-args*)
           ;; check_hd or check-hd both name lcert.formal-test.check-hd
           (map #(symbol (str "lcert.formal-test." (.replace (str %) "_" "-"))) *command-line-args*)
           lcert.formal-test/suites))
(apply require nss)
(let [{:keys [fail error]} (apply clojure.test/run-tests nss)]
  (shutdown-agents)
  (System/exit (if (zero? (+ fail error)) 0 1)))
