:- module(tabt,
          [ tab/3,
            tab_proof/3,
            xtab/3,
            xtab_proof/3,
            tab_search/3,
            xtab_search/3,
            ugrounding_term/1,
            admissible_term/1,
            term_policy/1,
            set_term_policy/1
          ]).

/**
 * Tab / Xtab inhabitant checker — Prolog reading of the inductive families
 * in docs/log/2026-09-05-tab-xtab-dependent-types.md (ADR-0001).
 *
 * A proof term is an explicit constructor tree.  `tab(Axioms, Branch, Proof)`
 * succeeds exactly when Proof is a well-formed inhabitant of Tab_α(Branch)
 * under the Willard 2020 Appendix rule profile (Rules 1–6 plus bounded
 * hybrids (a),(b)).  `xtab/3` adds the LEM/cut constructor `lem/2`.
 *
 * Formula syntax
 * --------------
 *   and(A,B), or(A,B), imp(A,B), neg(A)
 *   all(X, Body), ex(X, Body)           % Body may contain var(X)
 *   ball(X, S, Body), bex(X, S, Body)   % bounded quantifiers
 *   leq(T, S)                           % t ≤ s
 *   var(X), param(N)                    % bound variables / eigenparameters
 *   other ground Prolog terms           % atomic sentences / terms
 *
 * Proof-term functors (Tab)
 * -------------------------
 *   close(Phi)
 *   ax(Phi, P)
 *   and1(P), and2(P)
 *   dnn(P)
 *   neg_or(P), neg_imp(P), neg_and(P), neg_ex(P), neg_all(P)
 *   or(P1, P2), imp(P1, P2)
 *   all(Term, P), ex(Param, P)
 *   ball(Term, P), bex(Param, P)
 *
 * Proof-term functors (Xtab only)
 * -------------------------------
 *   of_tab(P)       — embed a Tab proof
 *   lem(Mho, P)     — write or(Mho, neg(Mho)) onto the branch
 */

:- use_module(library(lists)).

%% =====================================================================
%% Public wrappers
%% =====================================================================

/** tab_proof(+Axioms, +Psi, +Proof) — Tab-proof of Psi from Axioms. */
tab_proof(Axioms, Psi, Proof) :-
    tab(Axioms, [neg(Psi)], Proof).

/** xtab_proof(+Axioms, +Psi, +Proof) — Xtab-proof of Psi from Axioms. */
xtab_proof(Axioms, Psi, Proof) :-
    xtab(Axioms, [neg(Psi)], Proof).

%% =====================================================================
%% Tab: inductive family checker
%% =====================================================================

/**
 * tab(+Axioms, +Branch, +Proof)
 *
 * Check that Proof derives closure of every completion of Branch.
 * Ground proof terms are checked deterministically via functor dispatch.
 */
tab(_Axioms, Branch, close(Phi)) :-
    !,
    once(( member(Phi, Branch),
           member(neg(Phi), Branch)
         )).
tab(Axioms, Branch, ax(Phi, P)) :-
    !,
    memberchk(Phi, Axioms),
    tab(Axioms, [Phi|Branch], P).
tab(Axioms, Branch, and1(P)) :-
    !,
    once(member(and(Y, _), Branch)),
    tab(Axioms, [Y|Branch], P).
tab(Axioms, Branch, and2(P)) :-
    !,
    once(member(and(_, G), Branch)),
    tab(Axioms, [G|Branch], P).
tab(Axioms, Branch, dnn(P)) :-
    !,
    once(member(neg(neg(Y)), Branch)),
    tab(Axioms, [Y|Branch], P).
tab(Axioms, Branch, neg_or(P)) :-
    !,
    once(member(neg(or(Y, G)), Branch)),
    tab(Axioms, [and(neg(Y), neg(G))|Branch], P).
tab(Axioms, Branch, neg_imp(P)) :-
    !,
    once(member(neg(imp(Y, G)), Branch)),
    tab(Axioms, [and(Y, neg(G))|Branch], P).
tab(Axioms, Branch, neg_and(P)) :-
    !,
    once(member(neg(and(Y, G)), Branch)),
    tab(Axioms, [or(neg(Y), neg(G))|Branch], P).
tab(Axioms, Branch, neg_ex(P)) :-
    !,
    once(member(neg(ex(X, Body)), Branch)),
    tab(Axioms, [all(X, neg(Body))|Branch], P).
tab(Axioms, Branch, neg_all(P)) :-
    !,
    once(member(neg(all(X, Body)), Branch)),
    tab(Axioms, [ex(X, neg(Body))|Branch], P).
tab(Axioms, Branch, or(P1, P2)) :-
    !,
    once(member(or(Y, G), Branch)),
    tab(Axioms, [Y|Branch], P1),
    tab(Axioms, [G|Branch], P2).
tab(Axioms, Branch, imp(P1, P2)) :-
    !,
    once(member(imp(Y, G), Branch)),
    tab(Axioms, [neg(Y)|Branch], P1),
    tab(Axioms, [G|Branch], P2).
tab(Axioms, Branch, all(Term, P)) :-
    !,
    admissible_term(Term),
    once(member(all(X, Body), Branch)),
    subst_formula(X, Term, Body, Instance),
    tab(Axioms, [Instance|Branch], P).
tab(Axioms, Branch, ex(Param, P)) :-
    !,
    Param = param(_),
    once(member(ex(X, Body), Branch)),
    fresh_param(Param, Branch),
    subst_formula(X, Param, Body, Instance),
    tab(Axioms, [Instance|Branch], P).
tab(Axioms, Branch, ball(Term, P)) :-
    !,
    admissible_term(Term),
    once(member(ball(X, S, Body), Branch)),
    admissible_term(S),
    subst_formula(X, Term, Body, Instance),
    tab(Axioms, [imp(leq(Term, S), Instance)|Branch], P).
tab(Axioms, Branch, bex(Param, P)) :-
    !,
    Param = param(_),
    once(member(bex(X, S, Body), Branch)),
    admissible_term(S),
    fresh_param(Param, Branch),
    subst_formula(X, Param, Body, Instance),
    tab(Axioms, [and(leq(Param, S), Instance)|Branch], P).

%% =====================================================================
%% Xtab = Tab + lem
%% =====================================================================

xtab(Axioms, Branch, of_tab(P)) :-
    !,
    tab(Axioms, Branch, P).
xtab(Axioms, Branch, lem(Mho, P)) :-
    !,
    xtab(Axioms, [or(Mho, neg(Mho))|Branch], P).

%% =====================================================================
%% Term grammar (the bounded-quantifier guard rail)
%% =====================================================================
/**
 * Willard's complexity classes are relative to the TERM signature, not
 * absolute.  `Willard2005` printed p. 5 (image-verified, quotation register):
 * "a *term* t is defined to be a constant, variable or a U-Grounding function
 * symbol (whose input arguments are recursively defined terms).  Also, the
 * quantifiers in the wffs `forall v <= t Psi(v)` and `exists v <= t Psi(v)`
 * are called *bounded quantifiers*."  `Willard2011` Definition D.1(i)
 * (printed p. 38, image-verified) says the same of Delta*_0: quantifiers
 * "bounded in an arbitrary manner by terms employing the U-Grounding function
 * symbols".
 *
 * Consequence, and the reason this recogniser exists: a bound must be a TERM.
 * A relation defined in the object theory -- multiplication's graph, say, which
 * Willard2005 Eq. (4) gives as a Delta*_0 formula over Division -- can appear
 * in a formula but can never appear in a bound position.  So a signature
 * without a multiplication function symbol cannot express a bounded search of
 * size x*y, however freely the program defines Mult/3 as a predicate.
 *
 * Signature drift is real and is why this is a table rather than a hard-wired
 * grammar.  `Willard2005`/`Willard2011` U-Grounding: six non-growth Grounding
 * functions (Subtraction, Division, Root, Maximum, Logarithm, Count) plus the
 * two growth functions Addition and Double.  `Willard1993-TR` printed p. 3's
 * G_0 is a different eight (Addition, Subtraction, Division, StringCount,
 * Shift, Extract, Andreverse, Address) -- drift D10/D11 in the codification.
 * Only Addition, Subtraction and Division are common to both.
 */

:- dynamic term_policy_flag/1.

%!  term_policy(-Policy) is det.
%
%   `open` (default) accepts any Prolog term as a witness or bound, which is
%   the behaviour this file had before the recogniser existed.  `ugrounding`
%   enforces the U-Grounding term grammar.
term_policy(P) :-
    ( term_policy_flag(Q) -> P = Q ; P = open ).

%!  set_term_policy(+Policy) is det.
set_term_policy(P) :-
    must_be(oneof([open, ugrounding]), P),
    retractall(term_policy_flag(_)),
    assertz(term_policy_flag(P)).

%!  ugrounding_signature(?Functor, ?Arity) is nondet.
%
%   The eight U-Grounding function symbols.  The six non-growth ones first.
ugrounding_signature(sub,    2).
ugrounding_signature(divi,   2).
ugrounding_signature(root,   2).
ugrounding_signature(max,    2).
ugrounding_signature(log,    1).
ugrounding_signature(count,  1).
ugrounding_signature(add,    2).
ugrounding_signature(double, 1).

%!  ugrounding_term(+T) is semidet.
%
%   T is a term of the U-Grounding language: a constant, a variable, an
%   eigenparameter, or a U-Grounding function symbol applied to terms.
%   Note what is NOT here: no multiplication, and no left shift `x * 32^y`.
ugrounding_term(var(_))   :- !.
ugrounding_term(param(_)) :- !.
ugrounding_term(k(I))     :- !, integer(I), I >= 0.
ugrounding_term(N)        :- integer(N), !, N >= 0.
ugrounding_term(T) :-
    compound(T),
    functor(T, F, A),
    ugrounding_signature(F, A),
    T =.. [_|Args],
    forall(member(X, Args), ugrounding_term(X)).

%!  admissible_term(+T) is semidet.
%
%   The policy-dispatching check applied to universal-instantiation witnesses
%   and to bounded-quantifier bounds.
admissible_term(T) :-
    term_policy(P),
    ( P == open -> true ; ugrounding_term(T) ).

%% =====================================================================
%% Substitution and freshness
%% =====================================================================

/**
 * subst_formula(+Var, +Term, +FormulaIn, -FormulaOut)
 *
 * Replace free occurrences of var(Var) by Term.  Quantifiers that bind the
 * same variable name shadow the substitution in their bodies.
 */
subst_formula(Var, Term, Expr, Out) :-
    subst_expr(Var, Term, Expr, Out).

subst_expr(Var, Term, var(Var), Term) :- !.
subst_expr(Var, _Term, var(Other), var(Other)) :-
    Other \== Var,
    !.
subst_expr(Var, _Term, all(Var, Body), all(Var, Body)) :- !.
subst_expr(Var, _Term, ex(Var, Body), ex(Var, Body)) :- !.
subst_expr(Var, Term, ball(Var, S, Body), ball(Var, SOut, Body)) :-
    !,
    subst_expr(Var, Term, S, SOut).
subst_expr(Var, Term, bex(Var, S, Body), bex(Var, SOut, Body)) :-
    !,
    subst_expr(Var, Term, S, SOut).
subst_expr(Var, Term, Expr, Out) :-
    Expr =.. [F|Args],
    maplist(subst_expr(Var, Term), Args, ArgsOut),
    Out =.. [F|ArgsOut].

/**
 * fresh_param(+Param, +Branch)
 *
 * Willard's "newly introduced parameter": Param must not already occur
 * anywhere in the branch formulas.
 */
fresh_param(Param, Branch) :-
    \+ ( member(Phi, Branch),
         occurs_term(Param, Phi)
       ).

occurs_term(T, T) :- !.
occurs_term(T, Expr) :-
    compound(Expr),
    Expr =.. [_|Args],
    once(( member(A, Args),
           occurs_term(T, A)
         )).

%% =====================================================================
%% Shallow search (demo / tests only — not a completeness claim)
%% =====================================================================

tab_search(Axioms, Psi, Proof) :-
    between(1, 12, Depth),
    tab_goal(Axioms, [neg(Psi)], Depth, Proof),
    !.

xtab_search(Axioms, Psi, Proof) :-
    between(1, 12, Depth),
    xtab_goal(Axioms, [neg(Psi)], Depth, Proof),
    !.

tab_goal(_Axioms, Branch, Fuel, close(Phi)) :-
    Fuel > 0,
    member(Phi, Branch),
    memberchk(neg(Phi), Branch),
    !.
tab_goal(Axioms, Branch, Fuel, Proof) :-
    Fuel > 0,
    Fuel1 is Fuel - 1,
    tab_expand(Axioms, Branch, Fuel1, Proof).

tab_expand(Axioms, Branch, Fuel, ax(Phi, P)) :-
    member(Phi, Axioms),
    \+ memberchk(Phi, Branch),
    tab_goal(Axioms, [Phi|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, and1(P)) :-
    member(and(Y, _), Branch),
    \+ memberchk(Y, Branch),
    tab_goal(Axioms, [Y|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, and2(P)) :-
    member(and(_, G), Branch),
    \+ memberchk(G, Branch),
    tab_goal(Axioms, [G|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, dnn(P)) :-
    member(neg(neg(Y)), Branch),
    \+ memberchk(Y, Branch),
    tab_goal(Axioms, [Y|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, neg_or(P)) :-
    member(neg(or(Y, G)), Branch),
    Formula = and(neg(Y), neg(G)),
    \+ memberchk(Formula, Branch),
    tab_goal(Axioms, [Formula|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, neg_imp(P)) :-
    member(neg(imp(Y, G)), Branch),
    Formula = and(Y, neg(G)),
    \+ memberchk(Formula, Branch),
    tab_goal(Axioms, [Formula|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, neg_and(P)) :-
    member(neg(and(Y, G)), Branch),
    Formula = or(neg(Y), neg(G)),
    \+ memberchk(Formula, Branch),
    tab_goal(Axioms, [Formula|Branch], Fuel, P).
tab_expand(Axioms, Branch, Fuel, or(P1, P2)) :-
    member(or(Y, G), Branch),
    tab_goal(Axioms, [Y|Branch], Fuel, P1),
    tab_goal(Axioms, [G|Branch], Fuel, P2).
tab_expand(Axioms, Branch, Fuel, imp(P1, P2)) :-
    member(imp(Y, G), Branch),
    tab_goal(Axioms, [neg(Y)|Branch], Fuel, P1),
    tab_goal(Axioms, [G|Branch], Fuel, P2).

xtab_goal(Axioms, Branch, Fuel, of_tab(P)) :-
    tab_goal(Axioms, Branch, Fuel, P).
xtab_goal(Axioms, Branch, Fuel, lem(Mho, P)) :-
    Fuel > 1,
    Fuel1 is Fuel - 1,
    member(Mho, Branch),
    \+ memberchk(or(Mho, neg(Mho)), Branch),
    xtab_goal(Axioms, [or(Mho, neg(Mho))|Branch], Fuel1, P).
