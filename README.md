# wine-db
**wine-db** (for lack of a better name) is a backend intended for keeping track of your wine collection.
It was/is developed as a project in the course INF329 Advanced Functional Programming at UiB.

The application is built in haskell using libraries like [scotty](https://hackage.haskell.org/package/scotty), [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple), and [beam](https://hackage.haskell.org/package/beam-core).

### For developers

Ensure that [sqlite](https://www.sqlite.org/index.html) and [stack](https://docs.haskellstack.org/en/stable/README/) is installed.

**Running the server:**
```sh
stack run
```

**Compiling:**
```
stack build
```
