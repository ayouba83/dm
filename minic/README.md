
# PROJET DE COMPILATION

### Auteurs: Ayouba DEBA & Corentin LANIER

## Description du projet:
    Le but de ce projet est de réaliser un compilateur pour un sous-langage du langage C appelé Mini-C en appliquant non seulement ce que nous avons appris dans le cours de Compilation mais également nos connaissances de bases sur les langages de programmation et en particulier le langage C. Il faut retenir que dans les programmes minic de ce projet, toutes les declarations de variables sont faites en tête et on ne doit pas mélanger le code des fonctions et les déclarations de variables

## Description du travail réalisé:
### Les différentes étapes du projet:
    Pour arriver à la fin de notre projet, nous avons passé différentes étapes qui sont:
    - Réalisalisation d'un analyseur lexical pour le langage minimal donné
    - Réalisation d'un analyseur syntaxique pour le langage minimal donné
    - Réalisation d'un vérificateur de type pour le langage minimal donné
    - Réalisation d'un programme principal pour tester l'ensemble du projet pour le langage minimal donné 
    - Extention du langage en ajoutant de nouvelles possibilités
    - Raffinage de l'analyse
    - Réalisation d'un afficheur pour un programme minic donné par son arbre de syntaxe abstraite
    - Réalisation d'un interprète de programme minic
### Description des différentes étape:
    Pour commencer, nous avons déclaré tous les types dans un le fichier [minic_ast.ml] répresentant l'AST et qu'on utilisera dans les autres programmes.
### L'analyseur lexical [minic_lexer.ml]
    L'analyseur lexical prend en entré un fichier source contenant un programme minic et renvoie une sequence de lexèmes répresentant les mots du langage. Il fait la différence entre les mots clés et les autres mots et tous les commentaires sont ignorés. Tous les lexèmes (tokens) sont déclarés dans dans le parseur.

### L'analiseur syntaxique [minic_parser.ml]
    L'analyseur syntaxique vérifie si les mots se regroupent bien pour former des phrases minic, c'est-à-dire si le programme source respecte la syntaxe du langage minic, et renvoie dans ce cas l'arbre de syntaxe abstraite correspondant à ce programme. Dans le cas où le programme est mal typé l'analyseur syntaxique renvoie un message d'erreur et arrête l'exécution du programme d'analyse.
### Le vérificateur de type [minic_typechecker.ml]
    Le vérificateur de type prend en entré l'AST renvoyé par l'analyseur syntaxique et vérifie que le programme est bien typé. Si le programme est bien typé, l'exécution du vérificateur de type termine bien, sinon l'exécution échoue en affichant un message d'erreur.

### Le programme principal [minic.ml]
    Le programme principal permet de connecter les étapes précedentes. Il prend en entrée un fichier contenant le programme minic à analyser,  Il appelle d'abord l'analyseur lexical, puis l'analyseur syntaxique et affiche "Successfully parsed program" en cas de réussite de l'analyse syntaxique, puis enchaine avec la vérification de type. Si la vérification de type termine bien, alors le programme principal affiche "Successfully checked program". Dans le cas où l'analyseur syntaxique ou bien le verificateur de type echoue, le l'exécution du programme principal s'arrête et un message d'erreur est affiché.

### Les extensions du langage:
    Nous avons étendu le langage en ajoutant plusieurs fonctionnalités. Tout d'abord nous avons ajouté la possibilité de déclarer des variables initialisées ou non initialisées et de n'importe quel type (int ou bool): int x; bool y; int v = 2; bool t = true;

### 1) La boucle for:
    Nous avons ajouté dans le langage la possibilité de définir la boucle for qui correspond à un sucre syntaxique pour la boucle while. Nous avons préféré définir un nouveau symbole pour la bouble for dans l'AST pour que l'affichage de l'AST puisse être identique au programme source (mot par mot) au lieu de considérer la boucle for comme une sequence d'instructions (for(int i = 0; i!=0; i++) { instructions } <=> int i = 0; while(i!=0){ instructions i++;}). En suite dans chaque programme, nous avons ajouté le traitement correspondant au cas la boucle for.
### 2) Nouveaux opérateurs:
    Nous avons ajouté dans le langage la possibilité d'utiliser tout les opérateurs arithmétiques, logiques booléens, et bit à bit. Pour cela, nous avons introduit dans l'AST un symbole pour chaque chaque opérateur. En suite dans chaque programme, nous avons ajouté le traitement qui correspond à chaque nouvel opérateur.

### 3) Des tableaux statiques (homogènes):
    Nous avons ensuite ajouté dans le langage la possibilité de définir et manipuler des tableaux statiques (homogènes) à une dimension ou plusieurs. Le tableau doit être initialisé à la déclaration et la déclaration doit respecter la syntaxe du C suivante:
    [type] [nom] []...[] = { e1, ..., en }  où e1,...,en sont des expressions 
     Exemples: bool b[] = { true, false, true || true, !false, true}  
     int t[] = { 1, 2, 3 }  
     int t[][] = {{1},{2}}  
    Exemples qui ne marchent pas: int tab[5] = {1, 2, 2} car on n'utilise pas cette forme de déclaration .

    la valeur d'un tableau est donc un array (car statique et homogène) qui contient des expression, ainsi nous avons introduit dans l'AST un nouveau symbole (Array of expr array) qui correspont à la valeur d'un tableau. par exemple {1, 1+2, 3*12, -1} = Array(Cst 1, Add(1, 2), Mul(3, 12), Opp(1)). 
    Les tableaux peuvent être de n'importe quel type (tableau de booléen ou d'entier, ou tableau de tableau etc..), pour cela nous avons défini un nouveau constructeur de type pour les tableaux (Tab of typ) qui prend en paramètre le type des éléments dans le tableau. Mais nous avons été confronté à un problème, on sait qu'en C on ne peut pas renvoyer un tableau comme retour d'une fonction mais plutôt un pointeur vers ce tableau qui est celui de son premier élément (on ne peut pas renvoyer un int[] mais un int*) donc pour renvoyer un tableau en C, il faut renvoyer un pointeur vers son premier élément, pour cela nous avons ajouté à la définition des types, un constructeur de type pour un pointeur d'élément (ptr of typ) qui prend en paramètre le type de l'élément pointé. Ainsi lors de la vérification du bon typage du retour d'une fonction (Return), on distinguera le cas où la valeur rétournée est un tableau, et dans ce cas on vérifie que le type de retour de la fonction est bien un pointeur de type des éléments dans le tableau. 
    Pour la vérification du bon typage d'une déclaration de tableau, comme le tableau est vu comme un array d'expression, nous vérifions que toutes les expressions sont de même type, et dans ce cas on vérifie que le type des éléments contenu dans le tableau correspond au type indiqué lors de la déclaration du tableau.
    Exemples: {1, 2+3, false} <=> Array([|Cst 1, Add(2, 3), Bcst false|]) et cette expression est mal typé car Bcst false est de type Bool et Add(2, 3) est de type Int 
    int t[] = {true, false} est une déclaration mal typée car false est de type bool qui est different du type int.
    Pour pouvoir manipuler les tableau, nous avons ajouter dans le langage les accès en écriture et en lecture d'un tableau. Pour celà, nous avons introduit dans l'AST:
    Deux fonctions: Elm(pos, tab) qui correspond à l'élément à la position pos du tableau tab ( tab[pos] ) et len(tab) <=> (sizeof tab) qui renvoie la taille du tableau.
    Une instruction: Insert(pos, val, tab) qui correspond à l'affectation tab[pos] = val.

    Nous avons ajouté dans chaque programme la partie qui correspond au cas d'une déclaration de tableau ou encore de l'utilisation d'un tableau.

### Le Raffinage de l'Analyse:
    Pour cette partie, nous avons, en premier, affiché des erreurs de typage reconnues dans le typechecker. Pour celà, un fonction générique qui dit en résumé "error in \*operateur\* at line ..., \*type\* was given but \*type\* was expected" ou bien occasionellement certains messages plus spécifiques à certaines erreurs. La ligne indiquée alors correspond à la ligne affichée par l'ast, affiché par nos soins, du programme
    Puis une erreur syntaxique assez courante : l'oubli de \*;\* à la fin d'instruction dans le parser.

### L'Afficheur [printm.ml]
    Nous avons réalisé un afficheur qui affiche un programme minic donné par son AST.Pour tout programme en minic bien formé, quelque soit le degré de barocité avec lequel il a été écrit, on affiche le programme de façon correcte, en passant son AST en paramètre de l'afficheur. Celà est dû à la correction des programmes que nous réalisé.

### L'interprète [minic_interpreteur_imp.ml]
    Bien que pas terminé, car nous nous y étions pris en retard, il offre au lecteur une idée de la façon dont on exécute le programme entré, on execute les instructions dans la fonction "main" qui n'a aucun argument pour des raisons de simplification.
    Comme le typechecker est lancé avant l'interprêteur, on a pas besoin de se préoccuper des types des expressions pour les opérateurs et lors de la recherche d'une variable dans l'environnement, on sait qu'elle existe car sinon le typechecker aurait renvoyé une erreur en voyant qu'elle n'existait pas.

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
