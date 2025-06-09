Page wikipedia sur les relations de Green + les demigroupes :
https://fr.wikipedia.org/wiki/Relations_de_Green

Hyper intéressant : la notion de combinateurs dans les langages multidimensionnels comme APL, BQN, ou J, étendue aux langages de programmation classique et surtout aux parsers. C'est à dire, comment "génerer" des expressions à la manière d'instructions de préprocess avec ce genre de combinateurs.

Super article de wikipedia sur la PLT
https://en.wikipedia.org/wiki/Programming_language_theory

Type theory + Automata theory ?
Idée : Les types deviennent des contraintes sur les termes reconnus.

Ce sont que des idées mais je me demandais si on pouvait typecheck les macros, mais ća demanderait d'avoir une info sur les types au moment du parsing.
D'ou l'idée de combiner types + automates (parser) et donc on pourrait peut-être combiner les algos d'inférence de types classiques style HM avec un bottom up parser ???
https://www.reddit.com/r/ProgrammingLanguages/comments/7rtx2z/integrating_type_system_inside_parser/
[Context Sensitive Parsing for Programming Languages](https://www.sciencedirect.com/science/article/pii/S2590118422000697)
Problématique complete :
Implementing Type Checking and type inference algorithm at parsing time to increase efficiency (drop a constant) for Bottom-up LR parsers
ee

