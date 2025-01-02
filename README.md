# TIPE 2025
This repository holds my work regarding the french TIPE exam of 2025.

## The main idea
My idea for this project was : what if we tried to combine the parsing step with the typechecking (type-inference) step ? For example in a RDP (Recursive Descent Parser) and HM (Hindley-Milner) typesystem, type inference and parsing are very similar in the way they scan a tree from bottom to up (for the RDP, this is during the building phase of the AST) (PoC in the `project` directory in OCaml).

### Project details
For the PoC, I will create a little RDP for the following lambda-calculus grammar, described using EBNF :
```ebnf
program = { expr }
expr = app | abs | let-binding
app = term [{ term }]
abs = '\' id '.' expr
let-binding = 'let' id '=' expr 'in' expr
term = string | int | bool | id `@{self-explanatory}` | '(' expr ')'
```
