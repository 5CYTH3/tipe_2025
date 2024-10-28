# TIPE 2024
This repository holds my work regarding the french TIPE exam of 2024 (first year). I am still not sure if I will pursue in this subject for next year's TIPE. If so, I will update the name and data of this repository.

## The main idea
My idea for this 'research article' was : what if we tried to combine the parsing step with the typechecking (type-inferring) step ? For example in a RDP (Recursive Descent Parser) and HM (Hindley-Milner) typesystem, type inference and parsing are very similar in the way they scan a tree from bottom to up (for the RDP, this is during the building phase of the AST).
If that is indeed possible (PoC in the `project` directory in OCaml), for which grammars ?

### Project details
For the PoC, I will create a little RDP for the following lambda-calculus grammar, described using EBNF :
```ebnf
program = { expr }
expr = literal | app | abs | let-binding
literal = string | int | bool | id `@{self-explanatory}`
app = id [{ expr }]
abs = '\' id '.' expr
let-binding = 'let' id '=' expr 'in' expr
```
