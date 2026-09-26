(ns lcert.core
  "The public API of λᶜᵉʳᵗ₀ as a small programming language.

  Programs are EDN forms (see lcert.syntax for the surface syntax).  A program
  runs with a *budget* n: n tokens of the resource type ◇, named $1 .. $n.
  Certificates — trees of type R — cost one token per internal node.

    (check-program n form)   the explicit derivation of Θₙ ⊢ form : A
    (certify n form)         its derivation, code, type, node count, budget
    (check code type-form)   Check: does code certify the closed type?
    (run n form)             type check, then evaluate with n fresh tokens
    (certificate-literal code)  the term building code's tree from tokens

  Terms already in abstract syntax go through check-term, encode, type-code
  and check-code."
  (:require [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.typing :as t]
            [lcert.check :as c]
            [lcert.eval :as ev]
            [lcert.kernel :as k]))

(defn parse-program "A surface form, parsed in Θₙ." [n form]
  (s/parse-term (s/token-scope n) form))

(defn check-term "The derivation of Θₙ ⊢ t :¹ A for an abstract term t." [n t]
  (t/check-top n t))

(defn check-program [n form] (check-term n (parse-program n form)))

(defn encode "The code of a derivation." [d] (e/enc-deriv d))

(defn type-code "The code of an abstract type." [A] (e/enc-exp A))

(defn certify
  "Everything about the certificate of `form` at budget n."
  [n form]
  (let [d (check-program n form)
        code (encode d)]
    {:derivation d
     :code code
     :type (:type d)
     :nodes (k/nodes code)
     :budget (k/budget code)}))

(defn check-code "Check(code, type-code)." [code tcode] (c/check code tcode))

(defn check
  "Does `code` certify the closed type written as the surface form type-form?"
  [code type-form]
  (check-code code (type-code (s/parse-type [] type-form))))

(defn run "Type check form in Θₙ and evaluate it, erasing, with n tokens." [n form]
  (ev/run n (parse-program n form)))

(defn certificate-literal "The term building code's tree from $1 .. $k." [code]
  (c/certificate-literal code))
