# nets
Simple graph (graph theory-graph, not.. charts/plots) library for Scala.
Simple graph library for Scala. The library design is based primarily on
my prior research experience on large-scale graph analysis in the
context of online social networks. Currently the only plan is to have a
simple `Graph` class that handles directed, undirected, weighted, and
unweighted graphs. The algorithms provided are not intended to be
comprehensive.

** The code *compiles* but it can't really run because Atto (parsing)
depends on Scalaz 7.0.x and Nets uses Scalaz 7.1.x (Monoid instance for
`scalaz.==>>`). \o/ **

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
* The `Order` instance for `Edge` only considers the endpoints, not the weight.
  Seems a bit hack-y, I don't like it (see `Graph#getEdge`). Need to think
  of a better way..

## License
Please see LICENSE for licensing details.
