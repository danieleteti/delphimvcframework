"""Find constructs that do not compile on Delphi 10.2 Tokyo, the oldest supported version.

usage: python tools/tokyo_check.py sources lib/loggerpro lib/swagdoc/Source
       python tools/tokyo_check.py --self-test

Exit code 1 when something is found. What it looks for:
  - a string literal longer than 255 characters (E2056 before Delphi 12), deprecated messages included
  - an anonymous method that captures a const parameter passed by reference (E2555 on 10.2):
    records, TGUID, TValue, Variant, static arrays. Copy the parameter to a local and capture that.
  - a dynamic array parameter defaulting to '= []' (XE8 rejects it, Tokyo unverified): use '= nil'
  - inline var / const / for var (10.3), NameOf (13), multiline ''' literals (12),
    digit separators and binary literals (11)
Do not run it on lib/dmustache: mORMot code, full of {$ifdef}-guarded inline vars.
"""
import os
import re
import sys

sys.setrecursionlimit(100000)

RX_ID = re.compile(r'[A-Za-z_&][A-Za-z0-9_]*')
RX_NUM = re.compile(r'%[01_]+|\$[0-9A-Fa-f_]+|\d[\d_]*(\.\d[\d_]*)?([eE][+-]?\d+)?')
RX_SYM = re.compile(r':=|<=|>=|<>|\.\.|\S')
RX_CHAR = re.compile(r'#(\$[0-9A-Fa-f]+|\d+)')
RX_BYREF_TYPE = re.compile(r'\b([A-Za-z_]\w*)\s*(<[^>=;]*>)?\s*=\s*(packed\s+)?(record\b|array\s*\[)', re.I)

# RTL types passed by reference as a const parameter (records bigger than a pointer, Variant)
RTL_BYREF = {'tguid', 'tvalue', 'variant', 'olevariant', 'tpoint', 'trect', 'tformatsettings',
             'tsearchrec', 'ttimestamp', 'tsystemtime', 'tfiletime', 'tpair', 'tmethod', 'ttimespan',
             'tstopwatch', 'tvarrec', 'tmarshaller', 'tsize', 'tpointf', 'trectf'}
ROUTINE = {'procedure', 'function', 'constructor', 'destructor', 'operator'}
TYPE_WITH_END = {'record', 'class', 'object', 'interface'}
STATEMENT_START = {'for', ';', 'begin', 'then', 'else', 'do', 'try', 'finally', 'except', 'repeat'}


class Tok:
    __slots__ = ('kind', 'text', 'line', 'low')

    def __init__(self, kind, text, line):
        self.kind, self.text, self.line = kind, text, line
        # 'low' is what the parser compares: never set for a string, so a literal ';' is not a symbol
        self.low = text.lower() if kind in ('id', 'sym') else None


def tokenize(src):
    """Identifiers, symbols, numbers and string literals; comments dropped, directives kept as 'dir'."""
    toks, i, n, line = [], 0, len(src), 1
    while i < n:
        c = src[i]
        if c == '\n':
            line += 1; i += 1
        elif c in ' \t\r':
            i += 1
        elif src.startswith('//', i):
            j = src.find('\n', i); i = n if j < 0 else j
        elif c == '{' or src.startswith('(*', i):
            close = '}' if c == '{' else '*)'
            j = src.find(close, i); j = n if j < 0 else j + len(close)
            if src.startswith('{$', i):
                toks.append(Tok('dir', src[i:j], line))
            line += src.count('\n', i, j); i = j
        elif src.startswith("'''", i) and src[i + 3:i + 4] in ('\r', '\n'):
            j = src.find("'''", i + 3); j = n if j < 0 else j + 3
            toks.append(Tok('mlstr', src[i:j], line))
            line += src.count('\n', i, j); i = j
        elif c in "'#" and (c == "'" or RX_CHAR.match(src, i)):
            # one literal = a run of 'quoted' and #nn pieces with nothing between them
            start, pieces = line, []
            while i < n and src[i] in "'#":
                if src[i] == "'":
                    j = i + 1
                    while True:
                        k = src.find("'", j)
                        if k < 0:
                            k = n; pieces.append(src[j:k]); break
                        if src.startswith("''", k):
                            pieces.append(src[j:k] + "'"); j = k + 2
                        else:
                            pieces.append(src[j:k]); break
                    i = k + 1
                else:
                    m = RX_CHAR.match(src, i)
                    if not m:
                        break
                    pieces.append('?'); i = m.end()
            toks.append(Tok('str', ''.join(pieces), start))
        else:
            for kind, rx in (('id', RX_ID), ('num', RX_NUM), ('sym', RX_SYM)):
                m = rx.match(src, i)
                if m:
                    break
            toks.append(Tok(kind, m.group(0), line)); i = m.end()
    return toks


class Unit:
    def __init__(self, path, byref):
        self.toks = tokenize(open(path, encoding='utf-8-sig', errors='replace').read())
        self.byref = byref
        self.found = []
        self.bodies = set()      # (begin, end) token indexes of every routine body
        self._memo = {}

    def low(self, k):
        return self.toks[k].low if 0 <= k < len(self.toks) else None

    def report(self, k, msg):
        self.found.append((self.toks[k].line, msg))

    # --- structure -------------------------------------------------------------------------------

    def match_end(self, j):
        """Index of the 'end' closing the block opened at j."""
        depth = 0
        while j < len(self.toks):
            if self.toks[j].kind == 'id':
                v = self.low(j)
                if v in ('begin', 'case', 'try', 'asm', 'record') or self.opens_type(j):
                    depth += 1
                elif v == 'end':
                    depth -= 1
                    if depth == 0:
                        return j
            j += 1
        return len(self.toks) - 1

    def opens_type(self, j):
        """'class'/'object'/'interface' that starts a declaration closed by 'end' (not a forward one)."""
        return self.low(j) in TYPE_WITH_END and self.low(j - 1) in ('=', 'packed') \
            and self.low(j + 1) not in (';', 'of')

    def is_routine_header(self, j):
        return self.toks[j].kind == 'id' and self.low(j) in ROUTINE \
            and self.low(j - 1) in (';', 'implementation', 'class') and self.toks[j + 1].kind == 'id'

    def params(self, k):
        """toks[k] is '(': return ([(name, type, mode)], index after ')')."""
        groups, group, depth, j = [], [], 0, k
        while j < len(self.toks):
            v = self.low(j)
            if v == '(':
                depth += 1
                if depth == 1:
                    j += 1; continue
            elif v == ')':
                depth -= 1
                if depth == 0:
                    break
            if v == ';' and depth == 1:
                groups.append(group); group = []
            else:
                group.append(j)
            j += 1
        groups.append(group)
        result = []
        for g in groups:
            mode = ''
            if g and self.low(g[0]) in ('const', 'var', 'out'):
                mode, g = self.low(g[0]), g[1:]
            if g and self.low(g[0]) == '[':          # [ref]
                while g and self.low(g[0]) != ']':
                    g = g[1:]
                g = g[1:]
            colon = next((i for i, t in enumerate(g) if self.low(t) == ':'), None)
            if colon is None:
                continue
            rest = g[colon + 1:]
            eq = next((i for i, t in enumerate(rest) if self.low(t) == '='), len(rest))
            typ = ''.join(self.toks[t].text for t in rest[:eq])
            result += [(self.toks[t].text, typ, mode) for t in g[:colon] if self.toks[t].kind == 'id']
        return result, j + 1

    def header_end(self, k):
        """From a routine keyword, return (params, index of the ';' closing the header)."""
        h, params = k + 1, []
        while h < len(self.toks) and self.low(h) not in ('(', ';', ':'):
            h += 1
        if self.low(h) == '(':
            params, h = self.params(h)
        while h < len(self.toks) and self.low(h) != ';':
            h += 1
        return params, h

    def body(self, k):
        """From just after a header: skip declarations and nested routines, return (begin, end)
        of the body, None for forward/external."""
        if k not in self._memo:
            self._memo[k] = self._body(k)
        return self._memo[k]

    def _body(self, j):
        while j < len(self.toks):
            v = self.low(j)
            if v in ('forward', 'external'):
                return None
            if v in ('begin', 'asm'):
                e = self.match_end(j)
                self.bodies.add((j, e))
                return j, e
            if self.is_routine_header(j):
                _, h = self.header_end(j)
                nested = self.body(h + 1)
                j = h + 1 if nested is None else nested[1] + 1
            elif self.opens_type(j):
                j = self.match_end(j) + 1
            else:
                j += 1
        return None

    def anonymous_end(self, j):
        h = j + 1
        if self.low(h) == '(':
            _, h = self.params(h)
        while h < len(self.toks) and self.low(h) not in ('begin', 'var', 'const', 'type', 'procedure', 'function'):
            h += 1
        r = self.body(h)
        return r[1] if r else h

    # --- checks ----------------------------------------------------------------------------------

    def check(self):
        toks = self.toks
        for k, t in enumerate(toks):
            if t.kind == 'str' and len(t.text) > 255:
                self.report(k, 'string literal of %d chars (max 255 before Delphi 12)' % len(t.text))
            elif t.kind == 'mlstr':
                self.report(k, "multiline ''' literal (Delphi 12)")
            elif t.kind == 'num' and '_' in t.text:
                self.report(k, 'digit separator (Delphi 11)')
            elif t.kind == 'num' and t.text.startswith('%'):
                self.report(k, 'binary literal (Delphi 11)')
            elif t.low == 'nameof':
                self.report(k, 'NameOf (Delphi 13)')
            elif t.low == '=' and self.low(k - 1) == '>' and self.low(k + 1) == '[' and self.low(k + 2) == ']':
                q = k - 1
                while q > 0 and self.low(q) != '<':
                    q -= 1
                if self.low(q - 1) == 'tarray':
                    self.report(k, "dynamic array default '= []' (rejected by XE8, Tokyo unverified): use '= nil'")
        impl = next((k for k, t in enumerate(toks) if t.low == 'implementation'), None)
        if impl is None:
            return self.found
        k = impl
        while k < len(toks):
            if self.opens_type(k):
                k = self.match_end(k) + 1
            elif self.is_routine_header(k):
                k = self.check_routine(k)
            else:
                k += 1
        self.check_inline_declarations()
        return self.found

    def check_routine(self, k):
        params, h = self.header_end(k)
        r = self.body(h + 1)
        if r is None:
            return h + 1
        risky = {}
        for name, typ, mode in params:
            base = typ.split('<')[0].split('.')[-1].lower()
            if mode == 'const' and (base in self.byref or typ.lower().startswith('array[')):
                risky[name.lower()] = (name, typ)
        if risky:
            j = h + 1
            while j <= r[1]:
                if self.low(j) in ('procedure', 'function') and self.low(j - 1) in (':=', '(', ',', '=', 'exit', 'result', '['):
                    stop = self.anonymous_end(j)
                    for q in range(j, stop + 1):
                        if self.low(q) in risky and self.low(q - 1) != '.':
                            self.report(q, "anonymous method captures 'const %s: %s' (E2555 on 10.2): copy it to a local"
                                        % risky[self.low(q)])
                    j = stop + 1
                else:
                    j += 1
        return r[1] + 1

    def check_inline_declarations(self):
        """'var'/'const' starting a statement inside a body. An anonymous method's own var section
        follows ')' or a type name, never a statement start."""
        for b, e in sorted(self.bodies):
            paren = 0
            for k in range(b + 1, e):
                v = self.low(k)
                if v in ('(', '['):
                    paren += 1
                elif v in (')', ']'):
                    paren -= 1
                elif paren == 0 and v in ('var', 'const') and self.low(k - 1) in STATEMENT_START:
                    self.report(k, 'inline %s (10.3 Rio)' % v)


def byref_types(files):
    """RTL by-reference types plus every record and static array declared in the scanned folders."""
    names = set(RTL_BYREF)
    for folder in set(os.path.dirname(f) for f in files):
        for f in os.listdir(folder):
            if f.lower().endswith('.pas'):
                src = open(os.path.join(folder, f), encoding='utf-8-sig', errors='replace').read()
                names.update(m.group(1).lower() for m in RX_BYREF_TYPE.finditer(src))
    return names


def scan(paths):
    files = []
    for p in paths:
        if os.path.isdir(p):
            files += [os.path.join(p, f) for f in sorted(os.listdir(p)) if f.lower().endswith(('.pas', '.dpr'))]
        else:
            files.append(p)
    byref = byref_types(files)
    found = []
    for f in files:
        found += ['%s(%d): %s' % (f, line, msg) for line, msg in sorted(set(Unit(f, byref).check()))]
    return found


SELF_TEST = r"""unit T;
interface
type
  TRec = record A, B: Integer; end;
  TFlags = set of (fA, fB);
procedure P(const AValues: TArray<string> = []);
implementation
procedure Good(const ARec: TRec; const AFlags: TFlags; const AName: string);
var
  lRec: TRec;
begin
  lRec := ARec;
  Run(procedure begin Use(lRec, AFlags, AName, ';', 'begin'); end);
end;
procedure Bad(const ARec: TRec);
begin
  Run(procedure begin Use(ARec); end);
  for var I := 0 to 1 do ;
end;
const
  C = '%s';
end.
""" % ('x' * 256)


def self_test():
    import tempfile
    with tempfile.TemporaryDirectory() as d:
        path = os.path.join(d, 'T.pas')
        with open(path, 'w') as f:
            f.write(SELF_TEST)
        found = [line.split(': ', 1)[1] for line in scan([d])]
    expected = ["dynamic array default '= []' (rejected by XE8, Tokyo unverified): use '= nil'",
                "anonymous method captures 'const ARec: TRec' (E2555 on 10.2): copy it to a local",
                'inline var (10.3 Rio)',
                'string literal of 256 chars (max 255 before Delphi 12)']
    assert found == expected, found
    print('self-test OK')


if __name__ == '__main__':
    if sys.argv[1:] == ['--self-test']:
        self_test()
        sys.exit(0)
    if not sys.argv[1:]:
        sys.exit(__doc__)
    result = scan(sys.argv[1:])
    print('\n'.join(result) if result else 'tokyo_check: nothing found')
    sys.exit(1 if result else 0)
