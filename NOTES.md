Notes
=====

```ocaml
type 'a bintree =
  | Leaf of 'a
  | Node of 'a bintree * 'a bintree
```

On ne génère pas mais on voit comme :

```ocaml
type ('a, 'fixpoint) bintree_poly =
  | Leaf of 'a
  | Node of 'fixpoint * 'fixpoint
```

### Type du zipper

On dérive `bintree_poly` par rapport à `'fixpoint`. C'est particulier parce que
c'est l'emplacement où on a récursivement la structure. Ça nous donne le type
des ancètres :

```ocaml
type 'a bintree_ancestors =
  | Nil
  | Node0 of hole * 'a bintree * 'a bintree_ancestors
  | Node1 of 'a bintree * hole * 'a bintree_ancestors
```

Tout ça nous donne le zipper :

```ocaml
type 'a bintree_zipper = 'a bintree * 'a bintree_ancestors
```

### Types pour composer

Pour composer plus tard (eg. `'a list bintree`), il nous faut les dérivées par
rapport aux autres variables. Pour `'a`, on génère :

```ocaml
type 'a bintree_poly_zda =
  | Leaf of hole
```

- `da` = on dérive par rapport à `'a`
- `zda` = on re-multiplie par `z` = on “garde” le constructeur

Ça nous donne le type qui se compose bien (et qui ne mentionne pas le nom, parce
que, de l'extérieur, on ne le voit pas) :

```ocaml
type 'a bintree_d0 = 'a binree_poly_zda * 'a bintree_ancestors
```
