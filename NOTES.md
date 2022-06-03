Notes
=====

Example
-------

Consider the following type of trees that enforce a non-empty list of children in the `Node` constructor:
```ocaml
type tree =
  | Leaf
  | Node of tree * tree list
```

We can have a more “mathematical” view of this type as:
```
tree(z) = z + z.tree(z).list(z, tree(z))
```
We can also see it as:
```
tree(z) = P(z, tree(z))
P(z, t) = z + z.t.list(z, t)
```

```
∂z tree(z) = ∂z P(z, tree(z)) + ∂t P(z, tree(z)) ∂z tree(z)
```

```
∂z P(z, t) = 1 + t.list(z, t) + z.t.∂z list(z, t)
z∂z P(z, t) = tree(z) + z.t.z∂z list(z, t)
```

```
∂t P(z, t) = z.1.list(z,t) + z.t.∂a list(z, t)
```

```
tree•(z) = z∂z.tree(z)
         = z∂z P(z, tree(z)) + ∂t P(z, tree(z)) z∂z tree(z)
         = (tree(z) + z.tree(z).list•(z,tree(z))) . list(∂t P(z, tree(z)))
```










```ocaml
type 'a list = | Nil | Cons of 'a * 'a list
list(z, 'a) = Q(z, 'a, list(z, 'a))
Q(z, 'a, l) = z + z.'a.l
```
