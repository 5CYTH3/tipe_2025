We cannot use DFAs for programming languages as we need to "count" parenthesis to have syntactically valid arithmetic expressions. It goes the same for "begin"s and "end"s. Therefore, programming languages cannot be regular.

Piste : ma méthode se base sur un parsing de tree et un type judgement à chaque noeuds. Cela implique que ma grammaire de base doit pouvoir être représentée similairement à un arbre, et selon ce post de StackExchange, seules les CFGs peuvent être représentées de cette manière.
https://cs.stackexchange.com/questions/29323/are-context-free-grammars-the-only-ones-that-have-parsing-trees

Infos sur les connexions entre théorie des catégories et des types
https://en.wikipedia.org/wiki/Type_theory#Category_theory
