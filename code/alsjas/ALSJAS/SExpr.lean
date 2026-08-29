import Std

/-!
# Native S-expressions

This module defines the common structural carrier used by ALSJAS surface
syntax, system identities, proof trees, machine descriptions, and CLI values.
The representation deliberately contains no node identifiers or sharing
constructors: equality and traversal are properties of the complete tree.

The parser is total.  Its explicit fuel is initialized above the source length
and decreases at every mutually recursive parser call.  Fuel exhaustion is
therefore reported as data rather than hidden behind a partial definition.
-/

namespace ALSJAS

/-- A native syntax tree. Atoms contain decoded text; lists contain complete
subtrees and cannot encode aliases or back-references. -/
inductive SExpr where
  | atom : String → SExpr
  | list : List SExpr → SExpr
deriving BEq, Repr

namespace SExpr

/-- A structured parse failure. `offset` counts consumed Unicode scalar
values, which is sufficient for deterministic CLI diagnostics without making
byte positions part of the syntax semantics. -/
structure ParseError where
  offset : Nat
  message : String
deriving DecidableEq, BEq, Repr

/-- The concrete grammar intentionally fixes its whitespace set instead of
depending on locale-sensitive host behavior. -/
private def isSpace : Char → Bool
  | ' ' | '\t' | '\n' | '\r' => true
  | _ => false

/-- Characters permitted in the canonical unquoted spelling of an atom. -/
private def isPlain : Char → Bool
  | '(' | ')' | '"' | '\\' => false
  | character => !isSpace character

/-- Advance across insignificant whitespace while maintaining the diagnostic
offset. -/
private def skipSpace : Nat → List Char → Nat × List Char
  | offset, character :: rest =>
      if isSpace character then skipSpace (offset + 1) rest
      else (offset, character :: rest)
  | offset, [] => (offset, [])

/-- Consume an unquoted atom. The caller has already established that the
first character is legal, so the returned character list is nonempty. -/
private def takeAtom : List Char → List Char × List Char
  | [] => ([], [])
  | character :: rest =>
      if isPlain character then
        let (characters, trailing) := takeAtom rest
        (character :: characters, trailing)
      else
        ([], character :: rest)

/-- Decode a quoted atom. Only the canonical escape vocabulary is accepted;
unknown escapes are rejected rather than silently reinterpreted. -/
private def parseQuoted : Nat → Nat → List Char → List Char →
    Except ParseError (String × Nat × List Char)
  | 0, offset, _, _ =>
      .error { offset, message := "quoted atom exceeded parser fuel" }
  | _ + 1, offset, _, [] =>
      .error { offset, message := "unterminated quoted atom" }
  | _ + 1, offset, accumulator, '"' :: rest =>
      .ok (String.ofList accumulator.reverse, offset + 1, rest)
  | fuel + 1, offset, accumulator, '\\' :: escaped :: rest =>
      let decoded? : Option Char :=
        match escaped with
        | 'n' => some '\n'
        | 'r' => some '\r'
        | 't' => some '\t'
        | '"' => some '"'
        | '\\' => some '\\'
        | _ => none
      match decoded? with
      | some decoded => parseQuoted fuel (offset + 2) (decoded :: accumulator) rest
      | none => .error { offset, message := s!"unsupported escape \\{escaped}" }
  | _ + 1, offset, _, ['\\'] =>
      .error { offset, message := "unterminated escape" }
  | fuel + 1, offset, accumulator, character :: rest =>
      parseQuoted fuel (offset + 1) (character :: accumulator) rest

/- Parse one expression and its list tail. The mutually recursive functions
share a decreasing fuel budget solely to make totality syntactically evident
to Lean; successful parsing is still driven by the input tree. -/
mutual
  private def parseOne : Nat → Nat → List Char →
      Except ParseError (SExpr × Nat × List Char)
    | 0, offset, _ =>
        .error { offset, message := "expression exceeded parser fuel" }
    | _ + 1, offset, [] =>
        .error { offset, message := "expected expression" }
    | fuel + 1, offset, '(' :: rest => do
        let (elements, nextOffset, trailing) ← parseList fuel (offset + 1) rest
        pure (.list elements, nextOffset, trailing)
    | _ + 1, offset, ')' :: _ =>
        .error { offset, message := "unexpected closing parenthesis" }
    | fuel + 1, offset, '"' :: rest => do
        let (value, nextOffset, trailing) ←
          parseQuoted fuel (offset + 1) [] rest
        pure (.atom value, nextOffset, trailing)
    | _ + 1, offset, character :: rest =>
        if isPlain character then
          let (tail, trailing) := takeAtom rest
          let atomCharacters := character :: tail
          .ok (.atom (String.ofList atomCharacters),
            offset + atomCharacters.length, trailing)
        else
          .error { offset, message := s!"unexpected character {character}" }

  private def parseList : Nat → Nat → List Char →
      Except ParseError (List SExpr × Nat × List Char)
    | 0, offset, _ =>
        .error { offset, message := "list exceeded parser fuel" }
    | fuel + 1, offset, input =>
        let (nextOffset, significant) := skipSpace offset input
        match significant with
        | [] => .error { offset := nextOffset, message := "unterminated list" }
        | ')' :: rest => .ok ([], nextOffset + 1, rest)
        | _ => do
            let (head, afterHead, trailing) ←
              parseOne fuel nextOffset significant
            let (tail, afterTail, remainder) ←
              parseList fuel afterHead trailing
            pure (head :: tail, afterTail, remainder)
end

/-- Parse exactly one S-expression. Leading and trailing whitespace is
accepted; a second expression or unmatched delimiter is rejected. -/
def parse (source : String) : Except ParseError SExpr :=
  let characters := source.toList
  let (offset, significant) := skipSpace 0 characters
  match parseOne (characters.length + 1) offset significant with
  | .error failure => .error failure
  | .ok (expression, nextOffset, trailing) =>
      let (endOffset, remainder) := skipSpace nextOffset trailing
      match remainder with
      | [] => .ok expression
      | _ => .error { offset := endOffset, message := "unexpected trailing input" }

/-- Escape one atom character for the canonical quoted representation. -/
private def escape : Char → String
  | '\n' => "\\n"
  | '\r' => "\\r"
  | '\t' => "\\t"
  | '"' => "\\\""
  | '\\' => "\\\\"
  | character => String.singleton character

/-- Concatenate strings without importing an unrelated formatting library. -/
private def concatenate : List String → String :=
  List.foldl (· ++ ·) ""

/-- Canonical rendering quotes empty atoms and every atom containing syntax,
whitespace, control escapes, or a backslash. -/
private def renderAtom (value : String) : String :=
  let characters := value.toList
  if !characters.isEmpty && characters.all isPlain then
    value
  else
    "\"" ++ concatenate (characters.map escape) ++ "\""

/-- Render a complete tree in its unique canonical surface form. -/
def render : SExpr → String
  | .atom value => renderAtom value
  | .list elements =>
      "(" ++ String.intercalate " " (elements.map render) ++ ")"

instance : ToString SExpr where
  toString := render

end SExpr

end ALSJAS
