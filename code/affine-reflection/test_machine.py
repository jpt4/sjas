"""Behavioral tests for ADR-0011; run red before the implementation.

No test equates bounded enumeration with consistency. The unsafe extension
is a positive control: it must type and expose the precise diagonal cycle.
"""
import unittest
from itertools import product

from machine import (Atom, App, Quote, Var, Ty, BOT, UNIT, G, Arr, Box,
                     I, K, B, C, E, D, M, F, U, STAR, W,
                     app, abstract, check, infer, size, reducts, step,
                     normalize, h1, diagonal)


class MachineTests(unittest.TestCase):
    def test_affine_basis(self):
        cases = [(app(I, STAR), STAR), (app(K, STAR, E), STAR),
                 (app(B, E, D, Quote(STAR)), Quote(STAR)),
                 (app(C, K, E, STAR), STAR)]
        for source, expected in cases:
            self.assertEqual(normalize(source), expected)

    def test_reflection_and_internal_code_operators(self):
        self.assertEqual(normalize(app(E, Quote(STAR))), STAR)
        self.assertEqual(normalize(app(D, Quote(STAR))), Quote(Quote(STAR)))
        self.assertEqual(normalize(app(M, Quote(I), Quote(STAR))),
                         Quote(app(I, STAR)))
        self.assertEqual(normalize(app(E, app(M, Quote(I), Quote(STAR)))), STAR)

    def test_quotes_are_inert_but_cover_the_whole_language(self):
        quoted = Quote(app(E, Quote(STAR)))
        self.assertIsNone(step(quoted))
        self.assertEqual(normalize(app(E, quoted)), STAR)
        self.assertEqual(normalize(app(E, Quote(E))), E)
        self.assertTrue(check(Quote(E), Box(Arr(Box(BOT), BOT))))
        # This evaluates a quoted interpreter, then runs code through it.
        self.assertEqual(normalize(app(app(E, Quote(E)), Quote(STAR))), STAR)

    def test_consistency_and_pair_consistency_types(self):
        self.assertTrue(check(E, Arr(Box(BOT), BOT)))
        for a in (BOT, UNIT, G, Box(G), Arr(G, BOT), Box(Arr(UNIT, UNIT))):
            self.assertTrue(check(h1(), Arr(Box(a), Arr(Box(Arr(a, BOT)), BOT))))
        # Exercise the same pair-elimination computation at a nonempty result.
        p, q = Var('p'), Var('q')
        apply_code = abstract('p', abstract('q', app(app(E, q), app(E, p))))
        self.assertEqual(normalize(app(apply_code, Quote(STAR), Quote(I))), STAR)

    def test_naming_fixed_point_is_present(self):
        self.assertTrue(check(F, Arr(Arr(Box(G), BOT), G)))
        self.assertTrue(check(U, Arr(G, Arr(Box(G), BOT))))
        # Reduction itself does not call the type checker.
        self.assertEqual(step(app(U, app(F, I))), I)

    def test_decidable_checker_rejects_bad_inputs(self):
        for term, ty in [(STAR, BOT), (app(STAR, STAR), UNIT),
                         (Quote(STAR), Box(BOT)), (app(E, STAR), UNIT),
                         (Var('x'), UNIT), (Atom('unknown'), UNIT), (W, UNIT)]:
            self.assertFalse(check(term, ty))
        self.assertFalse(check(app(I, STAR), Box(UNIT)))
        # Occurs check: W W needs an infinite type, even in the unsafe variant.
        self.assertIsNone(infer(app(W, W), allow_w=True))
        self.assertFalse(check(Quote(W), Box(UNIT)))
        with self.assertRaises(ValueError):
            normalize(Quote(W))

    def test_checker_rejects_malformed_target_types(self):
        for bad in (None, Ty('arr'), Ty('box'), Ty('arr', (UNIT,)),
                    Ty('box', (UNIT, UNIT)), Ty('Bot', (UNIT,)),
                    Ty('unknown'), Ty('var'), Ty('var', (-1,))):
            self.assertFalse(check(E, bad), repr(bad))

    def test_affine_abstraction_refuses_duplication_and_open_quotes(self):
        x = Var('x')
        self.assertEqual(normalize(app(abstract('x', app(I, x)), STAR)), STAR)
        self.assertEqual(normalize(app(abstract('x', app(x, STAR)), I)), STAR)
        self.assertEqual(normalize(app(abstract('x', STAR), E)), STAR)
        with self.assertRaises(ValueError):
            abstract('x', app(x, x))
        with self.assertRaises(ValueError):
            abstract('x', Quote(x))

    def test_all_root_rules_strictly_decrease(self):
        # Arbitrary untyped arguments, with different sizes, test resource use.
        samples = (STAR, Quote(E), app(app(I, I), Quote(STAR)))
        for x, y, z in product(samples, repeat=3):
            sources = (app(I, x), app(K, x, y), app(B, x, y, z),
                       app(C, x, y, z), app(E, Quote(x)), app(D, Quote(x)),
                       app(M, Quote(x), Quote(y)), app(U, app(F, x)))
            for source in sources:
                targets = reducts(source, under_quotes=True)
                self.assertTrue(targets)
                for target in targets:
                    self.assertLess(size(target), size(source))

    def test_enumerated_preservation_and_no_bottom(self):
        # Exhaustive over trees <= 5 nodes on this stated finite alphabet.
        levels = {1: {I, K, B, C, E, D, M, F, U, STAR}}
        tested = 0
        for n in range(1, 6):
            if n > 1:
                levels[n] = {Quote(t) for t in levels[n-1]}
                for k in range(1, n-1):
                    levels[n].update(App(f, x) for f in levels[k]
                                     for x in levels[n-1-k])
            for term in levels[n]:
                self.assertFalse(check(term, BOT))
                ty = infer(term)
                if ty is None:
                    continue
                tested += 1
                nf = normalize(term)
                self.assertTrue(check(nf, ty))
                for target in reducts(term, under_quotes=True):
                    self.assertLess(size(target), n)
                    self.assertTrue(check(target, ty))
        self.assertGreater(tested, 600)

    def test_duplication_gives_a_typed_exact_cycle(self):
        omega = diagonal()
        self.assertFalse(check(omega, BOT))
        self.assertTrue(check(omega, BOT, allow_w=True))
        trace = [omega]
        for _ in range(4):
            trace.append(step(trace[-1], allow_w=True))
        self.assertEqual(trace[-1], omega)
        self.assertTrue(any(size(y) >= size(x) for x, y in zip(trace, trace[1:])))
        for term in trace:
            self.assertTrue(check(term, BOT, allow_w=True))
        with self.assertRaises(ValueError):
            normalize(omega)


if __name__ == '__main__':
    unittest.main(verbosity=2)
