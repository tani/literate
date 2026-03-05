import Lean

open Lean Elab Command
open Lean Parser

namespace LiterateLean

syntax (name := leanFence) "~~~lean" command* "~~~" : command

@[command_elab leanFence]
def elabLeanFence : CommandElab
  | `(command| ~~~lean $cmds* ~~~) => cmds.forM elabCommand
  | _ => throwError "invalid Lean fenced block"

declare_syntax_cat mdChunk
syntax ident : mdChunk
syntax str : mdChunk
syntax num : mdChunk
syntax ":" : mdChunk
syntax "//" : mdChunk
syntax "," : mdChunk
syntax "." : mdChunk
syntax ";" : mdChunk
syntax "!" : mdChunk
syntax "?" : mdChunk
syntax "(" : mdChunk
syntax ")" : mdChunk
syntax "[" : mdChunk
syntax "]" : mdChunk
syntax "<" : mdChunk
syntax ">" : mdChunk
syntax "-" : mdChunk
syntax "*" : mdChunk
syntax "/" : mdChunk
syntax "=" : mdChunk
syntax "_" : mdChunk

syntax (name := markdownHeading) "#" ident* : command
def markdownCodeSpanParser := leading_parser nameLit >> "`"
syntax (name := markdownCodeSpan) markdownCodeSpanParser : command
syntax (name := markdownInlineLine) mdChunk+ : command

def ignoreCommand : CommandElab := fun _ => pure ()

@[command_elab markdownHeading]
def elabMarkdownHeading : CommandElab := ignoreCommand

@[command_elab markdownCodeSpan]
def elabMarkdownCodeSpan : CommandElab
  | _ => pure ()

@[command_elab markdownInlineLine]
def elabMarkdownInlineLine : CommandElab := ignoreCommand

end LiterateLean
