# Installation et setup

## Installation du compilateur

Poru pouvoir faire fonctionner le compilateur, il faut avoir installé :
- OCaml, version 4.08 ou plus
- Le système de construction `dune` version 1.11 ou plus (installable avec `opam install dune`)
- `ocp-indent` et `ocamlfind` (installable avec `opam install ocp-indent ocamlfind`)

Avant toutes choses, il faut exécuter la commande
```bash
$ make deps
```
dans ce dossier, depuis un terminal. Vous pouvez tester que le programme `simple-parser-gen` à bien été installé à l’aide de la commande
```bash
$ simple-parser-gen --help
```
Il vous faut ensuite compiler le compilateur. Pour ce faire, il suffit d’exécuter la commande
```bash
$ make
```

Et voilà ! Vous avez maintenant le fichier executable `antsc`, votre compilateur !


## Comment compiler un fichier `.fml`

Une fois que vous avez codé votre fichier en langage `fml` (voir section "Le langage fml" pour apprendre à coder en `fml`), par exemple `strategie.fml`, il est très simple de le compiler en un fichier `.brain`. Pour cela, il faut lancer la commande
```bash
$ ./antsc strategie.fml -o strategie.brain
```
par défaut, si vous ne spécifiez pas la cible de sortie (`stratégie.brain`), votre programme compilé sera écrit dans le fichier `cervo.brain`. 

# Un langage de base

Le compilateur fourni compile un exemple de langage haut-niveau (les goto et les labels ne sont pas visibles) vers du fourmissembleur. Il a les features suivantes :
- supporte toutes les actions basiques que peut réaliser une fourmi. La compilation est telle que si une action échoue on continue l'éxécution du programme comme si l'action avait réussi ;
- des constructions spéciales movelese et pickelse pour les action Move et PickUp qui permettent d'essayer de bouger/récupérer de la nourriture et exécutent un code spécifique en cas d'échec de la manoeuvre ;
- des boucles while avec conditions true ou un sense de la fourmi

La grammaire exacte de ce langage est détaillée dans src/lang.grammar. Vous pouvez aussi jeter un oeil à silly.strat, une petite stratégie (peu intéressante) écrite dans ce langage.

# À vous de jouer !

Pour modifier le compilateur, vous avez juste à modifier deux fichiers : lang.grammar qui définit la grammaire de votre langage et antsc.ml qui compile ce langage vers le fourmissembleur.

N'hésitez pas à parcourir antsc.ml qui contient une pléthore de commentaires utiles pour vous aider dans la tâche de modifier le compilateur. Le "main" est tout en bas.

lang.grammar décrit une grammaire. Ce fichier s'interface avec une librairie de parsing automatique. Vous pouvez définir de nouveaux types de tokens, de nouveaux tokens, et les expressions de votre langage qui seront parsées vers ces tokens. Par exemple l.24 dans le type de token `expression` :
```
	| While				while (<condition>) do {<expression+;>}
```
signifie qu'on parsera un token While (de type `expression`) si on voit écrit "while" (un token de type `condition`) "do" {une suite de tokens de type `expression` séparés par des ;}. 

N'hésitez pas à comparer les fonctions [comp_xyz] de antsc.ml et le type [xyz] correspondant de lang.grammar pour mieux comprendre le lien entre la grammaire et l'AST que vous exploitez ensuite.


Un peu coincés sur le lancement ? Voilà quelques idées de départ :
-> rajoutez une structure if then else au langage
-> rajoutez des constructeurs avec gestion d'erreur pour les autres actions de fourmi qui peuvent échouer (vous pouvez vous inspirer de moveelse et pickelse)
-> optimisez la compilation ! Vos .brain sont sûrement remplis de Goto vers le label suivant. Pas très utile et ça ralentit vos fourmis...