import Lean

open Lean Elab Command Parser

namespace LiterateLean

syntax (name := leanFence) "~~~lean" command* "~~~" : command

@[command_elab leanFence]
def elabLeanFence : CommandElab
  | `(command| ~~~lean $cmds* ~~~) => cmds.forM elabCommand
  | _ => throwError "invalid Lean fenced block"

private def stopPrefixes : List String :=
  [ "#check", "#check_failure", "#eval", "#print", "#reduce", "#synth", "~~~lean", "~~~" ]

private def guardKeywords : List String :=
  [ "def ", "theorem ", "lemma ", "example ", "inductive ", "structure ", "class ", "instance "
  , "namespace ", "section ", "import ", "open ", "@[", "end " ]

private def shouldStopAt (c : ParserContext) (i : String.Pos.Raw) : Bool :=
  let remain := String.Pos.Raw.extract c.inputString i c.endPos
  stopPrefixes.any (fun p => remain.startsWith p)

private def markdownBlockGuard (c : ParserContext) (i : String.Pos.Raw) : Bool :=
  let remain := String.Pos.Raw.extract c.inputString i c.endPos
  guardKeywords.any (fun kw => remain.startsWith kw)

private partial def skipUntilLeanFenceFn (consumed : Bool) : ParserFn := fun c s =>
  let i := s.pos
  if shouldStopAt c i || (!consumed && markdownBlockGuard c i) then
    if consumed then s else s.mkUnexpectedError "expected markdown text"
  else if h : c.atEnd i then
    if consumed then s else s.mkEOIError
  else
    skipUntilLeanFenceFn true c (s.next' c i h)

private def markdownStartToken : Parser := leading_parser
  symbol "#" <|> symbol ">" <|> symbol "-" <|> symbol "*" <|> symbol "<" <|> symbol "$" <|>
  rawCh '`' <|> ident <|> rawIdent

@[command_parser]
def markdownBlock : Parser := leading_parser
  lookahead markdownStartToken >> withFn (fun _ => skipUntilLeanFenceFn false) skip

@[command_elab markdownBlock]
def elabMarkdownBlock : CommandElab := fun _ => pure ()

end LiterateLean
