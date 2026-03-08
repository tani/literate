import Lean

open Lean Elab Command Parser

namespace LiterateLean

private def forbiddenTilde : Parser :=
  withForbidden "```" (categoryParser `command 0)

syntax (name := leanFence) "```lean" forbiddenTilde* "```" : command

@[command_elab leanFence]
def elabLeanFence : CommandElab
  | `(command| ```lean $cmds* ```) => cmds.forM elabCommand
  | _ => throwError "invalid Lean fenced block"

private def startsWithAt (c : ParserContext) (i : String.Pos.Raw) (pref : String) : Bool :=
  (String.Pos.Raw.extract c.inputString i c.endPos).startsWith pref

private def isNewline (c : ParserContext) (i : String.Pos.Raw) : Bool :=
  if h : c.atEnd i then false else c.get' i h == '\n'

private partial def skipMarkdownUntilLeanFenceFn (lineStart consumed : Bool) : ParserFn := fun c s =>
  let i := s.pos
  if lineStart && startsWithAt c i "```lean" then
    if consumed then s else s.mkUnexpectedError "expected markdown text"
  else if h : c.atEnd i then
    if consumed then s else s.mkEOIError
  else
    let isNl := isNewline c i
    skipMarkdownUntilLeanFenceFn isNl true c (s.next' c i h)

private def unicodeRanges : List (Nat × Nat) := [
  -- Hangul Jamo
  (0x1100, 0x11FF),
  -- Enclosed Alphanumerics
  (0x2460, 0x24FF),
  -- Geometric Shapes
  (0x25A0, 0x25FF),
  -- CJK Radicals Supplement
  (0x2E80, 0x2EFF),
  -- Kangxi Radicals
  (0x2F00, 0x2FDF),
  -- Ideographic Description Characters
  (0x2FF0, 0x2FFF),
  -- CJK Symbols and Punctuation
  (0x3000, 0x303F),
  -- Hiragana
  (0x3040, 0x309F),
  -- Katakana
  (0x30A0, 0x30FF),
  -- Bopomofo
  (0x3100, 0x312F),
  -- Hangul Compatibility Jamo
  (0x3130, 0x318F),
  -- Kanbun
  (0x3190, 0x319F),
  -- Bopomofo Extended
  (0x31A0, 0x31BF),
  -- CJK Strokes
  (0x31C0, 0x31EF),
  -- Katakana Phonetic Extensions
  (0x31F0, 0x31FF),
  -- Enclosed CJK Letters and Months
  (0x3200, 0x32FF),
  -- CJK Compatibility
  (0x3300, 0x33FF),
  -- CJK Unified Ideographs Extension A
  (0x3400, 0x4DBF),
  -- Yijing Hexagram Symbols
  (0x4DC0, 0x4DFF),
  -- CJK Unified Ideographs
  (0x4E00, 0x9FFF),
  -- Yi Syllables
  (0xA000, 0xA48F),
  -- Yi Radicals
  (0xA490, 0xA4CF),
  -- Hangul Jamo Extended-A
  (0xA960, 0xA97F),
  -- Hangul Syllables
  (0xAC00, 0xD7AF),
  -- Hangul Jamo Extended-B
  (0xD7B0, 0xD7FF),
  -- CJK Compatibility Ideographs
  (0xF900, 0xFAFF),
  -- Halfwidth and Fullwidth Forms
  (0xFF00, 0xFFEF),
  -- Ideographic Symbols and Punctuation
  (0x16FE0, 0x16FFF),
  -- Kana Supplement
  (0x1B000, 0x1B0FF),
  -- Kana Extended-A
  (0x1B100, 0x1B12F),
  -- Small Kana Extension
  (0x1B130, 0x1B16F),
  -- Enclosed Alphanumeric Supplement
  (0x1F100, 0x1F1FF),
  -- Enclosed Ideographic Supplement
  (0x1F200, 0x1F2FF),
  -- CJK Unified Ideographs Extension B
  (0x20000, 0x2A6DF),
  -- CJK Unified Ideographs Extension C
  (0x2A700, 0x2B73F),
  -- CJK Unified Ideographs Extension D
  (0x2B740, 0x2B81F),
  -- CJK Unified Ideographs Extension E
  (0x2B820, 0x2CEAF),
  -- CJK Unified Ideographs Extension F
  (0x2CEB0, 0x2EBEF),
  -- CJK Unified Ideographs Extension I
  (0x2EBF0, 0x2EE5F),
  -- CJK Compatibility Ideographs Supplement
  (0x2F800, 0x2FA1F),
  -- CJK Unified Ideographs Extension G
  (0x30000, 0x3134F),
  -- CJK Unified Ideographs Extension H
  (0x31350, 0x323AF),
  -- CJK Unified Ideographs Extension J
  (0x323B0, 0x3347F)
]

elab "add_unicode_tokens" : command => do
  for (s, e) in unicodeRanges do
    for i in [s:e+1] do
      let ch := Char.ofNat i
      liftCoreM <| Lean.Parser.addToken ch.toString .global

add_unicode_tokens

private def isUnicode (c : Char) : Bool :=
  let v := c.val.toNat
  List.any unicodeRanges fun (s, e) => s ≤ v && v ≤ e

private def unicodeFn : ParserFn := satisfyFn isUnicode "Unicode letter-like character"

private def unicode : Parser := withFn (fun _ => unicodeFn) skip

private def markdownStartToken : Parser := leading_parser
  symbol "#" <|> symbol ">" <|> symbol "-" <|> symbol "*" <|> symbol "<" <|>
  symbol "$" <|> symbol "[" <|> symbol "(" <|> symbol "!" <|>
  symbol "+" <|> symbol "=" <|> symbol "_" <|> symbol "~" <|>
  symbol "|" <|> symbol ":" <|> symbol ";" <|> symbol "," <|> symbol "." <|>
  symbol "?" <|> symbol "/" <|> symbol "\\" <|> symbol "@" <|>
  symbol "{" <|> symbol "}" <|> symbol "&" <|> symbol "%" <|> symbol "^" <|>
  rawCh '`' <|> ident <|> rawIdent <|>
  numLit <|> strLit <|> charLit <|> scientificLit <|> unicode

private def markdownBlockFn : ParserFn := fun c s =>
  let i := s.pos
  if c.forbiddenTk? == some "```" then
    s.mkUnexpectedError "expected Lean command"
  else if startsWithAt c i "```lean" then
    s.mkUnexpectedError "expected markdown text"
  else
    skipMarkdownUntilLeanFenceFn true false c s

@[command_parser]
def markdownBlock : Parser := leading_parser
  lookahead markdownStartToken >> withFn (fun _ => markdownBlockFn) skip

@[command_elab markdownBlock]
def elabMarkdownBlock : CommandElab := fun _ => pure ()

end LiterateLean
