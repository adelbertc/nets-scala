# nets
Minimal graph (graph theory-graph, not.. charts/plots) library for Scala.
The library is designed primarily around my research work on large-scale
graph analysis in the context of online social networks. Currently
the only plan is to have a simple `Graph` class that handles directed,
undirected, weighted, and unweighted graphs. The algorithms provided are
not intended to be comprehensive.

## Design
* The data structure backing the graph is an adjacency list, as many of the
  graphs I work with are sparse.
* The adjacency list is `scalaz.==>>`, which is based on Haskell's
  `Data.Graph`, a balanced binary tree. In terms of dealing with large graphs,
  the logarithmic access leaves a bit to be desired. I will look into
  alternative data structures such as HAMTs and Radix Trees in the future.
* The "list" part of the adjacency list is an `IndexedSet`, a set type
  backed by a `Vector`.
  * Most of the time I am just building up a graph *once*
  * I rarely test for edge membership
  * The nature of random graph generation and random walks requires fast
    random neighbor selection. I gladly trade logarithimic membership tests
    for effectively constant random access.

## License
Please see LICENSE for licensing details.
