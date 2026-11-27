"""A finite-tree reflective affine combinator calculus (ADR-0011).

The object machine is the rewrite relation below, not the Python host. Its
checker accepts the complete syntax, including E itself. Quotation contains
syntax, never a Python closure. Python sharing has no object-level meaning:
size counts each occurrence, and W is rejected even inside inert quotations.

The type checker is an external algorithm. The object language has typed
code but does not yet represent or quantify over this checker as a program.
"""
from dataclasses import dataclass
from itertools import count


@dataclass(frozen=True)
class Atom:
    name: str


@dataclass(frozen=True)
class App:
    fn: object
    arg: object


@dataclass(frozen=True)
class Quote:
    body: object


@dataclass(frozen=True)
class Var:
    """Only for the affine bracket-abstraction helper; never machine syntax."""
    name: str


I, K, B, C, E, D, M, F, U, STAR, W = map(Atom, 'I K B C E D M F U star W'.split())
SAFE = frozenset('I K B C E D M F U star'.split())


def app(fn, *args):
    """Build left-associated application; this does not evaluate anything."""
    for arg in args:
        fn = App(fn, arg)
    return fn


def size(term):
    """Tree node count, including every node inside inert quoted code."""
    match term:
        case Atom(_) | Var(_):
            return 1
        case App(f, x):
            return 1 + size(f) + size(x)
        case Quote(t):
            return 1 + size(t)
        case _:
            raise ValueError('not a finite machine term')


def validate(term, allow_w=False):
    """Check syntax only, independently of typing and of normalization."""
    match term:
        case Atom(name) if name in SAFE or (name == 'W' and allow_w):
            return
        case App(f, x):
            validate(f, allow_w)
            validate(x, allow_w)
        case Quote(t):
            validate(t, allow_w)
        case _:
            raise ValueError('unknown constant, open term, or forbidden W')


def contract(term, allow_w=False):
    """One root contraction. No semantic oracle or type check is consulted."""
    match term:
        case App(Atom('I'), x):
            return x
        case App(App(Atom('K'), x), _):
            return x
        case App(App(App(Atom('B'), f), g), x):
            return app(f, app(g, x))
        case App(App(App(Atom('C'), f), x), y):
            return app(f, y, x)
        case App(Atom('E'), Quote(t)):
            return t
        case App(Atom('D'), Quote(t)):
            return Quote(Quote(t))
        case App(App(Atom('M'), Quote(f)), Quote(x)):
            return Quote(app(f, x))
        case App(Atom('U'), App(Atom('F'), t)):
            return t
        case App(App(Atom('W'), f), x) if allow_w:
            return app(f, x, x)
        case _:
            return None


def step(term, allow_w=False):
    """Leftmost outermost execution. Quoted payloads stay inert until E."""
    root = contract(term, allow_w)
    if root is not None:
        return root
    if isinstance(term, App):
        left = step(term.fn, allow_w)
        if left is not None:
            return App(left, term.arg)
        right = step(term.arg, allow_w)
        if right is not None:
            return App(term.fn, right)
    return None


def reducts(term, under_quotes=False):
    """All single-step safe reducts, for testing every contextual rule.

    Reducing under quotes is an optional *mathematical* strengthening of the
    termination result. The executable strategy deliberately does not do so.
    """
    out = []
    root = contract(term)
    if root is not None:
        out.append(root)
    if isinstance(term, App):
        out.extend(App(t, term.arg) for t in reducts(term.fn, under_quotes))
        out.extend(App(term.fn, t) for t in reducts(term.arg, under_quotes))
    elif isinstance(term, Quote) and under_quotes:
        out.extend(Quote(t) for t in reducts(term.body, under_quotes))
    return out


def normalize(term):
    """Total on finite safe terms; at most size(term)-1 rewrite steps.

    The assertion is a regression check, not the proof of termination. Each
    rule's algebraic decrease is established separately in the research note.
    There is intentionally no unsafe-W normalization API.
    """
    validate(term)
    while (next_term := step(term)) is not None:
        assert size(next_term) < size(term), 'a rule violated affine decrease'
        term = next_term
    return term


@dataclass(frozen=True)
class Ty:
    tag: str
    args: tuple = ()


BOT, UNIT, G = Ty('Bot'), Ty('Unit'), Ty('G')


def Arr(a, b):
    return Ty('arr', (a, b))


def Box(a):
    return Ty('box', (a,))


def _infer(term, allow_w=False, target=None):
    """First-order type unification; G remains rigid throughout.

    Each constant occurrence receives fresh scheme variables. The occurs
    check rejects infinite types. Requested schematic target variables are
    rigidified, so `check` verifies every instance of such a target, not just
    compatibility with one instance. No code execution occurs here.
    """
    validate(term, allow_w)
    serial, subst = count(), {}

    def fresh():
        return Ty('var', (next(serial),))

    def resolve(t):
        if t.tag == 'var' and t in subst:
            return resolve(subst[t])
        if t.tag in ('arr', 'box'):
            return Ty(t.tag, tuple(resolve(a) for a in t.args))
        return t

    def occurs(v, t):
        t = resolve(t)
        return v == t or (t.tag in ('arr', 'box') and
                          any(occurs(v, a) for a in t.args))

    def unify(a, b):
        a, b = resolve(a), resolve(b)
        if a == b:
            return
        if a.tag == 'var':
            if occurs(a, b):
                raise ValueError('infinite type')
            subst[a] = b
        elif b.tag == 'var':
            unify(b, a)
        elif a.tag == b.tag and a.tag in ('arr', 'box'):
            for x, y in zip(a.args, b.args):
                unify(x, y)
        else:
            raise ValueError('type mismatch')

    def scheme(name):
        a, b, c = fresh(), fresh(), fresh()
        return {
            'I': Arr(a, a),
            'K': Arr(a, Arr(b, a)),
            'B': Arr(Arr(b, c), Arr(Arr(a, b), Arr(a, c))),
            'C': Arr(Arr(a, Arr(b, c)), Arr(b, Arr(a, c))),
            'E': Arr(Box(a), a),
            'D': Arr(Box(a), Box(Box(a))),
            'M': Arr(Box(Arr(a, b)), Arr(Box(a), Box(b))),
            'F': Arr(Arr(Box(G), BOT), G),
            'U': Arr(G, Arr(Box(G), BOT)),
            'star': UNIT,
            'W': Arr(Arr(a, Arr(a, b)), Arr(a, b)),
        }[name]

    def walk(t):
        match t:
            case Atom(name):
                return scheme(name)
            case Quote(body):
                return Box(walk(body))
            case App(f, x):
                f_ty, x_ty, result = walk(f), walk(x), fresh()
                unify(f_ty, Arr(x_ty, result))
                return resolve(result)
        raise ValueError('invalid syntax')

    def rigidify(t):
        if not isinstance(t, Ty):
            raise ValueError('invalid target type')
        if t.tag == 'var':
            if len(t.args) != 1 or not isinstance(t.args[0], int) or t.args[0] < 0:
                raise ValueError('invalid type variable')
            return Ty('rigid', t.args)
        if t.tag in ('arr', 'box'):
            if len(t.args) != (2 if t.tag == 'arr' else 1):
                raise ValueError('invalid type constructor arity')
            return Ty(t.tag, tuple(rigidify(a) for a in t.args))
        if t.tag not in ('Bot', 'Unit', 'G') or t.args:
            raise ValueError('invalid target type')
        return t

    result = walk(term)
    if target is not None:
        unify(result, rigidify(target))
    return resolve(result)


def infer(term, allow_w=False):
    """Return a principal simple type, or None for rejected syntax/typing."""
    try:
        return _infer(term, allow_w)
    except ValueError:
        return None


def check(term, target, allow_w=False):
    """The external decidable checker for this entire finite-tree language."""
    if not isinstance(target, Ty):
        return False
    try:
        _infer(term, allow_w, target)
        return True
    except ValueError:
        return False


def free(term):
    match term:
        case Var(name):
            return {name}
        case App(f, x):
            return free(f) | free(x)
        case Quote(t):
            return free(t)
        case Atom(_):
            return set()
    raise ValueError('invalid abstraction syntax')


def abstract(name, term):
    """Eliminate an affine variable into B, C, K, I; no hidden S or W.

    This is construction-time syntax sugar. It cannot be used by an object
    program to copy an input or to quote a variable.
    """
    if isinstance(term, Quote) and free(term):
        raise ValueError('quotation must be closed')
    if name not in free(term):
        return app(K, term)
    if term == Var(name):
        return I
    if isinstance(term, App):
        left, right = name in free(term.fn), name in free(term.arg)
        if left and right:
            raise ValueError('contraction is not affine')
        if left:
            return app(C, abstract(name, term.fn), term.arg)
        return app(B, term.fn, abstract(name, term.arg))
    raise ValueError('cannot abstract an open quotation')


def h1():
    """One type-schematic term: lambda p q. (E q) (E p)."""
    return app(B, app(C, E), E)


def diagonal():
    """A Bot-typed four-step loop after adding only W.

    h = F (W (B U E)); omega = U h <h>.
    Its construction duplicates finite source text externally; its execution
    requires W to duplicate code supplied as an unknown argument.
    """
    h = app(F, app(W, app(B, U, E)))
    return app(U, h, Quote(h))


def pretty(term):
    match term:
        case Atom(name) | Var(name):
            return name
        case Quote(t):
            return '<' + pretty(t) + '>'
        case App(f, x):
            return '(' + pretty(f) + ' ' + pretty(x) + ')'
    raise ValueError('invalid syntax')


if __name__ == '__main__':
    example = app(app(E, Quote(E)), app(M, Quote(I), Quote(STAR)))
    print('Evaluate an interpreter and run composed code through it:')
    while True:
        print(f'  {size(example):3} nodes  {pretty(example)}')
        target = step(example)
        if target is None:
            break
        example = target
    print('H0 type checks:', check(E, Arr(Box(BOT), BOT)))
    print('H1 type checks:', check(h1(), Arr(Box(G), Arr(Box(Arr(G, BOT)), BOT))))
    omega = diagonal()
    print('With W, a checked Bot term repeats after four steps:')
    print('  checked Bot:', check(omega, BOT, allow_w=True))
    term = omega
    for _ in range(5):
        print(f'  {size(term):3} nodes  {pretty(term)}')
        term = step(term, allow_w=True)
