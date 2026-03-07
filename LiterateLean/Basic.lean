import Lean

open Lean Elab Command Parser

namespace LiterateLean

private def forbiddenTilde : Parser :=
  withForbidden "~~~" (categoryParser `command 0)

syntax (name := leanFence) "~~~lean" forbiddenTilde* "~~~" : command

@[command_elab leanFence]
def elabLeanFence : CommandElab
  | `(command| ~~~lean $cmds* ~~~) => cmds.forM elabCommand
  | _ => throwError "invalid Lean fenced block"

private def startsWithAt (c : ParserContext) (i : String.Pos.Raw) (pref : String) : Bool :=
  (String.Pos.Raw.extract c.inputString i c.endPos).startsWith pref

private def isNewline (c : ParserContext) (i : String.Pos.Raw) : Bool :=
  if h : c.atEnd i then false else c.get' i h == '\n'

private partial def skipMarkdownUntilLeanFenceFn (lineStart consumed : Bool) : ParserFn := fun c s =>
  let i := s.pos
  if lineStart && startsWithAt c i "~~~lean" then
    if consumed then s else s.mkUnexpectedError "expected markdown text"
  else if h : c.atEnd i then
    if consumed then s else s.mkEOIError
  else
    let isNl := isNewline c i
    skipMarkdownUntilLeanFenceFn isNl true c (s.next' c i h)

private def markdownStartToken : Parser := leading_parser
  symbol "#" <|> symbol ">" <|> symbol "-" <|> symbol "*" <|> symbol "<" <|> symbol "$" <|>
  rawCh '`' <|> ident <|> rawIdent

private def markdownBlockFn : ParserFn := fun c s =>
  let i := s.pos
  if c.forbiddenTk? == some "~~~" then
    s.mkUnexpectedError "expected Lean command"
  else if startsWithAt c i "~~~lean" then
    s.mkUnexpectedError "expected markdown text"
  else
    skipMarkdownUntilLeanFenceFn true false c s

@[command_parser]
def markdownBlock : Parser := leading_parser
  lookahead markdownStartToken >> withFn (fun _ => markdownBlockFn) skip

@[command_elab markdownBlock]
def elabMarkdownBlock : CommandElab := fun _ => pure ()

end LiterateLean
