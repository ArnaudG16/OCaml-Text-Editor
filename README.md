# OCaml Text Editor 

OCaml Text Editor est un éditeur de texte minimaliste qui explore l'utilisation de structures de données purement fonctionnelles, spécifiquement les Zippers, pour implémenter un buffer d'édition efficace et robuste.

## Description

Ce projet a pour objectif de manipuler des structures de données avancées (Zippers) pour permettre à l'éditeur d'interpréter et d'appliquer des actions clavier basiques : déplacement du curseur, insertion de caractères, suppression et retour à la ligne. L'interface graphique est réalisée à l'aide du module `Graphics` d'OCaml.

## Fonctionnalités

L'éditeur propose les fonctionnalités essentielles suivantes :

* Édition de texte : Insertion de caractères et gestion du retour à la ligne (`Entrée`).
* Navigation : Déplacement fluide du curseur (Haut, Bas, Gauche, Droite).
    * Navigation intelligente : Lors des déplacements verticaux, le curseur tente de conserver sa position horizontale, même si les lignes sont de longueurs différentes.
    * Passage de ligne : Le curseur passe automatiquement à la ligne suivante ou précédente lorsqu'il atteint les extrémités d'une ligne.
* Suppression avancée :
    * Support des touches `Suppr` et `Backspace`.
    * Fusion de lignes : Si la suppression intervient au début ou à la fin d'une ligne, l'éditeur fusionne automatiquement la ligne courante avec la précédente ou la suivante.
* Raccourcis : Déplacement rapide au début ou à la fin de la ligne courante.

## Architecture : Les Zippers

Le cœur de l'application repose sur le type `zipper`, permettant un accès constant à l'élément sous le curseur et des modifications locales à moindre coût.

### Modélisation
Le buffer textuel est modélisé par une double structure de zipper :

1.  La Ligne (`line`) : Un zipper de caractères `(char, cursor) zipper`.
2.  Le Buffer (`buffer`) : Un zipper de lignes `(line, line) zipper`.

```ocaml
type ('a, 'b) zipper = {
  before: 'a list;  (* Éléments précédents (ordre inversé) *)
  current: 'b;      (* Élément courant *)
  after: 'a list;   (* Éléments suivants *)
  pos: int          (* Position du curseur *)
}
