import Lean

open Lean Elab Command Parser

namespace LiterateLean

syntax (name := leanFence) "~~~lean" command* "~~~" : command

@[command_elab leanFence]
def elabLeanFence : CommandElab
  | `(command| ~~~lean $cmds* ~~~) => cmds.forM elabCommand
  | _ => throwError "invalid Lean fenced block"

syntax (name := markdownHeading) "#" ident* : command
def markdownCodeSpanParser : Parser := leading_parser
  rawCh '`' >> (ident <|> rawIdent <|> nameLit) >> rawCh '`'
syntax (name := markdownCodeSpan) markdownCodeSpanParser : command

private def isUnsafeToken (s : String) : Bool :=
  s = "/-" || s = "/--" || s = "/-!" || s = "-/" || s = "\"" || s = "'" || s = "`"

private def isSymbolLikeChar (c : Char) : Bool :=
  !c.isWhitespace && !c.isAlpha && !c.isDigit && c ≠ '"' && c ≠ '\'' && c ≠ '`'

private def escapeLeanString (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    if c = '\\' then acc ++ "\\\\"
    else if c = '"' then acc ++ "\\\""
    else acc.push c

private def registerToken (cat : Name) (tok : String) : CommandElabM Unit := do
  if isUnsafeToken tok then
    pure ()
  else
    let cmdText := s!"syntax \"{escapeLeanString tok}\" : {cat}"
    match Parser.runParserCategory (← getEnv) `command cmdText with
    | .ok stx =>
        try
          elabCommand stx
        catch _ =>
          pure ()
    | .error _ => pure ()

private def isAsciiOpChar (c : Char) : Bool :=
  c = ':' || c = '=' || c = '-' || c = '>' || c = '<' || c = '|' || c = '&' || c = '+' ||
  c = '*' || c = '/' || c = '.' || c = '!' || c = '?' || c = '%' || c = '^' || c = '~'

private def mkTokensOfLen (alphabet : Array Char) (len : Nat) : Array String :=
  let atoms := alphabet.map Char.toString
  let rec go : Nat → Array String
    | 0 => #[]
    | 1 => atoms
    | n + 1 =>
      let prev := go n
      prev.foldl (init := #[]) fun acc p =>
        atoms.foldl (init := acc) fun acc a => acc.push (p ++ a)
  go len

private def asciiOpAlphabet : Array Char := Id.run do
  let mut out : Array Char := #[]
  for n in [33:127] do
    let c := Char.ofNat n
    if isAsciiOpChar c then
      out := out.push c
  out

private def registerSymbolRange (cat : Name) (lo hi : Nat) : CommandElabM Unit := do
  if hi < lo then
    throwError "invalid range: stop must be >= start"
  for n in [lo:hi+1] do
    let c := Char.ofNat n
    if isSymbolLikeChar c then
      registerToken cat c.toString

private def registerAsciiSymbolCombos (cat : Name) (maxLen : Nat) : CommandElabM Unit := do
  if maxLen < 2 then
    throwError "invalid max length: must be >= 2"
  if maxLen > 6 then
    throwError "invalid max length: must be <= 6"
  for len in [2:maxLen+1] do
    for tok in mkTokensOfLen asciiOpAlphabet len do
      registerToken cat tok

syntax (name := registerMarkdownSymbolsFor)
  "register_markdown_symbols_for" ident : command

@[command_elab registerMarkdownSymbolsFor]
def elabRegisterMarkdownSymbolsFor : CommandElab
  | `(command| register_markdown_symbols_for $cat:ident) => do
      let cat := cat.getId
      registerSymbolRange cat 33 126
      registerAsciiSymbolCombos cat 3
      registerSymbolRange cat 0x2100 0x214F
      registerSymbolRange cat 0x2190 0x21FF
      registerSymbolRange cat 0x2200 0x22FF
      registerSymbolRange cat 0x27C0 0x27EF
      registerSymbolRange cat 0x2980 0x29FF
      registerSymbolRange cat 0x2A00 0x2AFF
  | _ => throwUnsupportedSyntax

declare_syntax_cat markdownSym
register_markdown_symbols_for markdownSym

def markdownInlineGuard : Parser := leading_parser
  notFollowedBy (symbol "def") "def" >>
  notFollowedBy (symbol "theorem") "theorem" >>
  notFollowedBy (symbol "lemma") "lemma" >>
  notFollowedBy (symbol "example") "example" >>
  notFollowedBy (symbol "inductive") "inductive" >>
  notFollowedBy (symbol "structure") "structure" >>
  notFollowedBy (symbol "class") "class" >>
  notFollowedBy (symbol "instance") "instance" >>
  notFollowedBy (symbol "namespace") "namespace" >>
  notFollowedBy (symbol "section") "section" >>
  notFollowedBy (symbol "import") "import" >>
  notFollowedBy (symbol "open") "open" >>
  notFollowedBy (symbol "@[") "@[" >>
  notFollowedBy (symbol "~~~lean") "~~~lean" >>
  notFollowedBy (symbol "~~~") "~~~" >>
  notFollowedBy (symbol "~") "~" >>
  notFollowedBy (symbol "end") "end"

def markdownHashToken : Parser := leading_parser "#" >> optional ident
def markdownDollarToken : Parser := leading_parser "$" >> optional ident

def markdownInlineToken : Parser := leading_parser
  rawCh '`' <|> markdownHashToken <|> markdownDollarToken <|> ident <|> rawIdent <|>
  strLit <|> numLit <|> scientificLit <|> charLit <|> nameLit <|>
  categoryParser `markdownSym maxPrec

def markdownInlineLineParser : Parser := leading_parser
  markdownInlineGuard >> many1 markdownInlineToken

syntax (name := markdownInlineLine) markdownInlineLineParser : command

def ignoreCommand : CommandElab := fun _ => pure ()

@[command_elab markdownHeading]
def elabMarkdownHeading : CommandElab := ignoreCommand

@[command_elab markdownCodeSpan]
def elabMarkdownCodeSpan : CommandElab := ignoreCommand

@[command_elab markdownInlineLine]
def elabMarkdownInlineLine : CommandElab := ignoreCommand

end LiterateLean
