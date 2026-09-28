:- module(tabt_test, []).

/**
 * plunit suite for Tab / Xtab inhabitant checking (ADR-0001).
 *
 * These tests fix the intended behavior of the inductive-family encoding:
 * well-formed proof terms succeed; ill-formed terms fail.
 */

:- use_module(tabt).
:- use_module(library(plunit)).

:- begin_tests(tabt_closure).

test(close_complementary_pair) :-
    tab([], [p, neg(p)], close(p)).

test(close_rejects_non_complementary) :-
    \+ tab([], [p, q], close(p)).

test(tab_proof_lem_identity_without_cut) :-
    %% Prove or(p, neg(p)) by pure Tab (2020 Rules 2 then 1 / dnn / close).
    %% Root: neg(or(p,neg(p)))
    %% neg_or -> and(neg(p), neg(neg(p)))
    %% and1 -> neg(p); and2 -> neg(neg(p)); dnn -> p; close.
    Proof = neg_or(and1(and2(dnn(close(p))))),
    tab_proof([], or(p, neg(p)), Proof).

:- end_tests(tabt_closure).

:- begin_tests(tabt_propositional).

test(and_elimination_both_ways) :-
    tab([], [and(p, q), neg(p)], and1(close(p))),
    tab([], [and(p, q), neg(q)], and2(close(q))).

test(or_requires_both_branches) :-
    tab([], [or(p, q), neg(p), neg(q)],
        or(close(p), close(q))).

test(or_rejects_single_branch_proof) :-
    \+ tab([], [or(p, q), neg(p), neg(q)], or(close(p))).

test(imp_split) :-
    tab([], [imp(p, q), p, neg(q)],
        imp(close(p), close(q))).

test(dnn) :-
    tab([], [neg(neg(p)), neg(p)], dnn(close(p))).

test(neg_and_to_or) :-
    %% neg(and(p,q)) pushes or(neg(p),neg(q)); close against p and q.
    tab([], [neg(and(p, q)), p, q],
        neg_and(or(close(p), close(q)))).

:- end_tests(tabt_propositional).

:- begin_tests(tabt_axioms_quantifiers).

test(axiom_onto_branch) :-
    tab([a], [neg(a)], ax(a, close(a))).

test(forall_instantiate) :-
    tab([], [all(x, p(var(x))), neg(p(c))],
        all(c, close(p(c)))).

test(exists_fresh_parameter) :-
    %% Fresh param(0) instantiates a self-contradictory matrix.
    tab([], [ex(x, and(p(var(x)), neg(p(var(x)))))],
        ex(param(0), and1(and2(close(p(param(0))))))).

test(exists_rejects_non_fresh_parameter) :-
    %% param(0) already occurs on the branch, so it is not fresh.
    \+ tab([], [ex(x, and(p(var(x)), neg(p(var(x))))), p(param(0))],
           ex(param(0), and1(and2(close(p(param(0))))))).

test(bounded_exists_hybrid) :-
    %% bex writes and(leq(p0,s), and(p(p0),neg(p(p0)))); then and-split/close.
    tab([], [bex(x, s, and(p(var(x)), neg(p(var(x)))))],
        bex(param(0),
            and2(and1(and2(close(p(param(0)))))))).

test(bounded_forall_hybrid) :-
    tab([], [ball(x, s, p(var(x))), neg(imp(leq(c, s), p(c)))],
        ball(c, close(imp(leq(c, s), p(c))))).

:- end_tests(tabt_axioms_quantifiers).

:- begin_tests(tabt_xtab).

test(xtab_includes_tab) :-
    xtab([], [p, neg(p)], of_tab(close(p))).

test(lem_writes_excluded_middle) :-
    %% Branch holds neg(or(mho,neg(mho))). LEM writes the complementary
    %% disjunction; embed Tab close. Pure Tab has no lem/2 constructor.
    Proof = lem(mho, of_tab(close(or(mho, neg(mho))))),
    xtab([], [neg(or(mho, neg(mho)))], Proof).

test(tab_rejects_lem_constructor) :-
    \+ tab([], [neg(or(mho, neg(mho)))],
           lem(mho, of_tab(close(or(mho, neg(mho)))))).

test(xtab_proof_wrapper) :-
    xtab_proof([], or(p, neg(p)),
               of_tab(neg_or(and1(and2(dnn(close(p))))))).

:- end_tests(tabt_xtab).

:- begin_tests(tabt_term_grammar).

%% The bounded-quantifier guard rail.  Willard2005 p. 5: a bound must be a
%% TERM, and terms are built only from the U-Grounding function symbols.  A
%% relation may be defined freely and still never occupy a bound position.

test(policy_defaults_to_open) :-
    tabt:term_policy(open).

test(ugrounding_accepts_grounding_terms) :-
    tabt:ugrounding_term(var(x)),
    tabt:ugrounding_term(param(0)),
    tabt:ugrounding_term(k(2)),
    tabt:ugrounding_term(add(var(x), double(k(1)))),
    tabt:ugrounding_term(divi(log(var(y)), k(2))).

test(ugrounding_rejects_multiplication) :-
    \+ tabt:ugrounding_term(mult(var(x), var(y))),
    \+ tabt:ugrounding_term(add(var(x), mult(var(y), var(z)))),
    %% left shift by a variable amount is the operation concatenation needs
    \+ tabt:ugrounding_term(shl(var(x), var(y))).

test(ugrounding_rejects_bare_atoms) :-
    \+ tabt:ugrounding_term(c),
    \+ tabt:ugrounding_term(s).

test(strict_policy_rejects_product_bound,
     [ setup(tabt:set_term_policy(ugrounding)),
       cleanup(tabt:set_term_policy(open)) ]) :-
    %% Same bounded-forall step as tabt_axioms_quantifiers, but with a bound
    %% that is a product.  Rejected because the bound is not a term.
    B = mult(var(y), var(z)),
    \+ tab([], [ball(x, B, p(var(x))), neg(imp(leq(k(1), B), p(k(1))))],
           ball(k(1), close(imp(leq(k(1), B), p(k(1)))))).

test(strict_policy_accepts_additive_bound,
     [ setup(tabt:set_term_policy(ugrounding)),
       cleanup(tabt:set_term_policy(open)) ]) :-
    B = add(var(y), var(z)),
    tab([], [ball(x, B, p(var(x))), neg(imp(leq(k(1), B), p(k(1))))],
        ball(k(1), close(imp(leq(k(1), B), p(k(1)))))).

test(open_policy_still_accepts_atoms) :-
    %% The pre-existing behaviour is unchanged under the default policy.
    tab([], [ball(x, s, p(var(x))), neg(imp(leq(c, s), p(c)))],
        ball(c, close(imp(leq(c, s), p(c))))).

:- end_tests(tabt_term_grammar).

:- begin_tests(tabt_search).

test(search_finds_lem_identity) :-
    once(tab_search([], or(p, neg(p)), Proof)),
    tab_proof([], or(p, neg(p)), Proof).

test(xtab_search_returns_checked_proof) :-
    once(xtab_search([], or(p, neg(p)), Proof)),
    xtab_proof([], or(p, neg(p)), Proof).

:- end_tests(tabt_search).
