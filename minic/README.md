
# PROJET DE COMPILATION

### Auteurs: Ayouba DEBA & Corentin LANIER

## Description du projet:
Le but de ce projet est de réaliser un compilateur pour un sous-langage du langage c appélé mimic en appliquant non seulement ce que nous avons appris dans le cours de Compilation mais également nos connaissances de bases sur les langages de programmation et en particulier le langage C.

## Description du travail réalisé:
### Les differentes étapes du projet:
Pour arriver à la fin de notre projet, nous avons passer differentes étapes qui sont:
- Réalisalisation d'un analyseur lexical pour le langage minimal donné
- Réalisation d'un analyseur syntaxique pour le langage minimal donné
- Réalisation d'un verificateur de type pour le langage minimal donné
- Réalisation d'un programme principale pour tester l'ensemble du pour le langage minimal donnéprojet
- Extention du langage en ajoutant de nouvelles possibilités
- Raffinage de l'analyse
- Réalisation d'un afficheur pour un programme minic donné par son arbre de syntaxe abstraite
- Réalisation d'un interprète de programme minic
### Description des differentes étape:
Pour commencer, nous avons déclaré tous les types dans un le fichier [minic_ast.ml] répresentant l'AST et qu'on utilisera dans les autros programmes.
### L'analyseur lexical [minic_lexer.ml]
L'analyseur l'exical prend en entré un fichier source contenant un programme minic et renvoie une sequense de lexènes répresentant les mots du langage. Il fait la difference entre les mots clés et les autres mots et tous les commentaires sont ignorés. Toutes les lexènes (tokens) sont déclarés dans dans le parseur.

### L'analiseur syntaxique [minic_parser.ml]
L'analyseur syntaxique prend en entré une 

### Le verificateur de type [minic_typechecker.ml]


### Le programme principale [minic.ml]


### Les extensions du langage:
### 1) La boucle for:
    



### 2) Nouveaux opérateurs:


### 3) Des tableaux statiques (homogènes):


### Le Raffinage de l'Analyse:


### L'Afficheur [printm.ml]


### L'interprète [.....]


## Outils pour compiler le projet:
Pour compiler ce projet vous avez besoin de :
- ocaml
- menhir
- dune
Chacun peut être installé à l'aide d'opam, ou d'apt-get sous Debian/Ubuntu ou Windows Subsystem for Linux.

## Pour compiler :
Placez-vous dans le reprertoir dm/minic/ et tapez la ligne de commande suivante: 

> dune build

L'exécutable est créé dans le répertoire dm/minic/_build. Plus précisément, il s'agit de : dm/minic/_build/default/minic.exe

## Test sur un fichier source :
Des fichiers de test sont donnés dans le repertoire dm/minic/tests, mais vous pouvez aussi créer de nouveaux fichiers sources dans ce repertoire pour tester vos programmes minic. \
Pour tester sur un fichier source [tests/nom_du_fichier], tapez la commande suivante (en supposant que vous vous êtes placé dans le repertoire dm/minic):

>./_build/default/minic.exe tests/nom_du_fichier


## Conseil pour progresser dans le projet :
(a) choisir un petit aspect du langage source pas encore traité (par exemple : une certaine forme d'expression ou d'instruction)
(b) créer un ou plusieurs nouveaux fichiers de tests intégrant ce nouvel aspect (et rien d'autre qui n'aurait pas encore été traité)
(c) ajouter dans minic_parser.mly les déclarations de nouveaux lexèmes nécessaires
(d) compléter minic_lexer.mll pour reconnaître ces nouveaux lexèmes
(e) compléter minic_parser.mly pour reconnaître la nouvelle forme d'expression ou d'instruction
(f) compléter minic_typechecker.ml pour traiter le nouveau cas
(g) compiler le programme et vérifier qu'il traite correctement les tests
Puis recommencer jusqu'à avoir traité l'ensemble du langage Mini-C.


Conservez bien tous les tests que vous avez élaborés. Il faut les vérifier de temps à autre, et on vous demandera de les fournir avec votre rendu.