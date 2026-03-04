import Lean

open Lean Elab Command

namespace Literate

syntax (name := leanFence) "~~~lean" command* "~~~" : command

@[command_elab leanFence]
def elabLeanFence : CommandElab
  | `(command| ~~~lean $cmds* ~~~) => cmds.forM elabCommand
  | _ => throwError "invalid Lean fenced block"

syntax (name := markdownHeading) "#" ident* : command
syntax (name := markdownTextGeneral) ident+ : command

def ignoreCommand : CommandElab := fun _ => pure ()

@[command_elab markdownHeading]
def elabMarkdownHeading : CommandElab := ignoreCommand

@[command_elab markdownTextGeneral]
def elabMarkdownTextGeneral : CommandElab := ignoreCommand

end Literate
