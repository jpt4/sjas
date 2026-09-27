"""Generate child / setKid for Exp from its constructor table in syntax.clj,
and splice them into conv.clj between the position markers."""
import re, sys
base = sys.argv[1] if len(sys.argv) > 1 else 'formal/lcert/formal'
syn = open(base + '/syntax.clj').read()
blk = syn[syn.index("(a/inductive Exp []"):syn.index(";; lift k c e")]
blk = re.sub(r";[^\n]*", "", blk)
spec = [(n, re.findall(r"\[(\w+) (\w+)\]", f)) for n, f in re.findall(r"\((\w+)((?: \[\w+ \w+\])*)\)", blk)]
spec = [(n, fs) for n, fs in spec if n not in ("a",)]
def pat(n, fs): return "(%s %s)" % (n, " ".join(f for f, _ in fs)) if fs else n
def mk(n, fs, vals): return "(Exp.%s %s)" % (n, " ".join(vals)) if fs else "Exp." + n
child = ["(a/defn child [e :- Exp, i :- Nat] (Option Exp)", "  (match e"]
setk = ["(a/defn setKid [e :- Exp, i :- Nat, x :- Exp] Exp", "  (match e"]
for n, fs in spec:
    exps = [f for f, t in fs if t == "Exp"]
    if exps:
        body = "(Option.none Exp)"
        for k in reversed(range(len(exps))):
            body = "(if (= i %d) (Option.some Exp %s) %s)" % (k, exps[k], body)
        child.append("    [%s %s]" % (pat(n, fs), body))
        vals, k = [], 0
        for f, t in fs:
            if t == "Exp": vals.append("(if (= i %d) x %s)" % (k, f)); k += 1
            else: vals.append(f)
        setk.append("    [%s %s]" % (pat(n, fs), mk(n, fs, vals)))
    else:
        child.append("    [%s (Option.none Exp)]" % pat(n, fs))
        setk.append("    [%s %s]" % (pat(n, fs), mk(n, fs, [f for f, _ in fs])))
child[-1] += "))"; setk[-1] += "))"
code = "\n".join(child) + "\n\n" + "\n".join(setk) + "\n"
p = base + '/conv.clj'
s = open(p).read()
a = s.index("(a/defn child ["); b = s.index(";; Paths: getP")
s = s[:a] + code + "\n" + s[b:]
open(p, 'w').write(s)
print(len(spec), "constructors")
