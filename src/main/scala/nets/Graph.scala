package nets

import scala.collection.immutable.Seq

import scalaz.{ ==>>, Order }

final class Graph[A, W] private(
    private val adjacencyList: A ==>> IndexedSet[Edge[A, W]],
    val isDirected: Boolean) {

  /* Vertex-centric functions */
  def degree(node: A)(implicit A: Order[A]): Option[Int] = adjacencyList.lookup(node).map(_.size)
  def nodes: List[A] = adjacencyList.keys
  def order: Int = adjacencyList.size
  def vertices: List[A] = nodes

  /* Edge-centric functions */
  def edges: List[Edge[A, W]] = 
    adjacencyList.toList.flatMap { case (_, nbors) => nbors.toList }

  def size: Int = {
    val bidirectionalEdges = adjacencyList.fold(0)((_, ns, acc) => acc + ns.size)
    if (isDirected) bidirectionalEdges else bidirectionalEdges / 2
  }

  val isUndirected: Boolean = !isDirected
}

object Graph {
}
