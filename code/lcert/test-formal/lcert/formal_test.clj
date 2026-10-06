(ns lcert.formal-test
  "The formal suite (ADR-0006), by namespace.  Each suite namespace below
  requires one formal namespace (kernel-checking every declaration in it)
  and tests that the expected constants exist and that false variants are
  rejected, so the statements are not vacuous.  bin/test-formal runs them
  all, in this order (the formal namespaces' dependency order); with
  arguments it runs only the named suites.")

(def suites
  '[lcert.formal-test.usage
    lcert.formal-test.syntax
    lcert.formal-test.skel
    lcert.formal-test.conv
    lcert.formal-test.judgment
    lcert.formal-test.carrier
    lcert.formal-test.den
    lcert.formal-test.sem
    lcert.formal-test.model
    lcert.formal-test.syntactic
    lcert.formal-test.subst
    lcert.formal-test.mono
    lcert.formal-test.skeletons
    lcert.formal-test.splitting
    lcert.formal-test.unfold
    lcert.formal-test.fundamental
    lcert.formal-test.recsyn
    lcert.formal-test.substitution
    lcert.formal-test.skof
    lcert.formal-test.conversion
    lcert.formal-test.vweaken
    lcert.formal-test.derivations
    lcert.formal-test.outer
    lcert.formal-test.lemma36
    lcert.formal-test.section4
    lcert.formal-test.section4b
    lcert.formal-test.strengthen
    lcert.formal-test.eval
    lcert.formal-test.encode
    lcert.formal-test.convcase
    lcert.formal-test.prop410
    lcert.formal-test.theorem4
    lcert.formal-test.erase
    lcert.formal-test.prop434
    lcert.formal-test.check
    lcert.formal-test.section4c
    lcert.formal-test.check-hd
    lcert.formal-test.check-skj
    lcert.formal-test.check-dt
    lcert.formal-test.check-der
    lcert.formal-test.check-agree
    lcert.formal-test.check-spec
    lcert.formal-test.enclabels
    lcert.formal-test.certenc
    lcert.formal-test.selfjust
    lcert.formal-test.uskel
    lcert.formal-test.funde
    lcert.formal-test.theorem4e
    lcert.formal-test.safety52
    lcert.formal-test.rint
    lcert.formal-test.enc46
    lcert.formal-test.lemma46a
    lcert.formal-test.theorem46
    lcert.formal-test.cor46
    lcert.formal-test.enc46w
    lcert.formal-test.enc46f7
    lcert.formal-test.ri-convcase
    lcert.formal-test.ri-conversion
    lcert.formal-test.ri-den
    lcert.formal-test.ri-fundamental
    lcert.formal-test.ri-lemma36
    lcert.formal-test.ri-model
    lcert.formal-test.ri-mono
    lcert.formal-test.ri-outer
    lcert.formal-test.ri-recsyn
    lcert.formal-test.ri-sem
    lcert.formal-test.ri-splitting
    lcert.formal-test.ri-subst
    lcert.formal-test.ri-substitution
    lcert.formal-test.ri-unfold
    lcert.formal-test.ri-vweaken])
