# recover-rtti

Recover run-time type information from the GHC heap. The key function in this
library is

```haskell
classify :: a -> Classifier a
```

which recovers type information about values about which we know nothing (in
particular, no type class constraints). One example use case is the following
`showAnything` function:

```haskell
showAnything :: a -> String
```

We test that the result of `showAnything` is equal to the result of regular
`show` for a large range of data types (including user-defined ones that the
library is not aware of).

Obviously there are limitations; in particular, `UNPACK`ed fields in
user-defined data types cannot be shown (at least, not in a human-readable
manner). There may be other gotchas as well; this library is primarily intended
for debugging.

This library is in early stages of development.
