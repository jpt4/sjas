(ns lcert.tutorial-test
  "Runs every example of TUTORIAL.md, so the tutorial cannot drift from the
  language.

  Each ```clojure block of the tutorial is read as Clojure forms and
  evaluated in order, in a scratch namespace.  A line `;; => X` after a form
  states its expected value.  X is data, compared with =.  `;; => :type-error`
  instead says the form must fail with a type error, and `;; => :error` that
  it must fail with any error the language reports (a parse error, say).  Forms with no
  expectation are evaluated for their effect, such as a def."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def tutorial-file "TUTORIAL.md")

(defn- blocks
  "The text of each ```clojure block."
  [text]
  (map second (re-seq #"(?s)```clojure\n(.*?)```" text)))

(defn- expectation-marked
  "Rewrite `;; => X` lines into (expect X) marker forms."
  [block]
  (str/replace block #"(?m)^\s*;; => (.*)$" "(lcert.tutorial-test/expect $1)"))

(defn- read-all [s]
  (let [r (java.io.PushbackReader. (java.io.StringReader. s))]
    (loop [acc []]
      (let [f (read {:eof ::eof} r)]
        (if (= f ::eof) acc (recur (conj acc f)))))))

(defn- expect-form? [f] (and (seq? f) (= 'lcert.tutorial-test/expect (first f))))

(defn- scratch-ns []
  (let [nm (gensym "lcert.tutorial-scratch")
        n (create-ns nm)]
    (binding [*ns* n]
      (refer-clojure)
      (require '[lcert.core :as lc] '[lcert.pa :as pa] '[lcert.examples :as ex]
               '[lcert.eval :as ev]))
    n))

(defn- run-form [n f]
  (try {:value (binding [*ns* n] (eval f))}
       (catch clojure.lang.ExceptionInfo e {:error (ex-data e) :message (.getMessage e)})))

(deftest every-tutorial-example-holds
  (let [text (slurp tutorial-file)
        n (scratch-ns)
        checked (atom 0)]
    (doseq [[bi block] (map-indexed vector (blocks text))]
      (let [forms (read-all (expectation-marked block))]
        (loop [fs forms last-form nil last-result nil]
          (when-let [f (first fs)]
            (if (expect-form? f)
              (let [want (second f)]
                (swap! checked inc)
                (testing (str "block " bi ": " (pr-str last-form))
                  (case want
                    :type-error (is (= :lcert/type-error (get-in last-result [:error :type])) (pr-str last-result))
                    :error (is (contains? last-result :error) (pr-str last-result))
                    (is (= want (:value last-result)) (pr-str last-result))))
                (recur (rest fs) last-form last-result))
              (recur (rest fs) f (run-form n f)))))))
    (testing "the tutorial states expectations at all"
      (is (< 20 @checked)))))
