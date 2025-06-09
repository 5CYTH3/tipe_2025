1. Avril 2024 - Écriture du système classique en ocaml, dans un premier temps pour une grammaire dans le style de Lisp puis pour le lambda calcul simplement typé.
2. Octobre 2024 - Écriture du système fusionné et optimisations de la fonction liée à la règle d'application de fonction.
3. Décembre 2024 - Optimisation via continuation passing style (CPS) pour profiter de récursion terminale.
4. Janvier 2025 - Découverte des travaux de Joseph Gil et Ori Roth [7] et de leur correspondance dans le cadre du système 'fluent'.
5. Février 2025 - Travail de recherche d'outils de formalisation via des automates d'arbres (livre Tree Automatas, technique and applications) pour palier au problème de stockage du contexte et d'arité des états.
6. Avril 2025 - Tests sur de larges échantillons de données, génération de n numéraux de church, application successive de fonctions.
7. Juin 2025 - Échec des tests sur des échantillons plus larges que 4000 à cause de stack overflow. Tentative de contournement en utilisant le CPS et en effectuant une trampolinisation, ce qui fausse les tests.
