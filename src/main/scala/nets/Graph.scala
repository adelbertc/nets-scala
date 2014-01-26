package nets

import scala.collection.immutable.Seq

import scalaz.{ ==>>, Order }
import scalaz.syntax.order._

import spire.algebra.{ Rig }

final class Graph[A, W] private(
    private val adjList: A ==>> IndexedSet[Edge[A, W]],
    val isDirected: Boolean) {

  /* Vertex-centric functions */
  def degree(node: A)(implicit A: Order[A]): Option[Int] =
    adjList.lookup(node).map(_.size)
  def neighbors(node: A)(implicit A: Order[A]): Option[IndexedSet[Edge[A, W]]] =
    adjList.lookup(node)
  def nodes: List[A] = adjList.keys
  def order: Int = adjList.size
  def vertices: List[A] = nodes

  /* Edge-centric functions */
  def edges: List[Edge[A, W]] = 
    adjList.toList.flatMap { case (_, nbors) => nbors.toList }

  def size: Int = {
    val bidirectionalEdges = adjList.fold(0)((_, ns, acc) => acc + ns.size)
    if (isDirected) bidirectionalEdges else bidirectionalEdges / 2
  }

  val isUndirected: Boolean = !isDirected
}

object Graph {
  def fromDirectedEdges[A : Order, W](es: Seq[Edge[A, W]]): Graph[A, W] = {
    val adjacencyList =
      es.foldLeft(==>>.empty[A, IndexedSet[Edge[A, W]]]) { (map, edge) =>
        if (edge.from === edge.to)
          map
        else if (map.member(edge.from))
          map.insert(edge.from, IndexedSet.singleton(edge))
        else
          map.adjust(edge.from, _.insert(edge))
      }
    new Graph(adjacencyList, true)
  }

  def fromUndirectedEdges[A : Order, W : Rig](es: Seq[Edge[A, W]]): Graph[A, W] = {
    val adjacencyList =
      es.foldLeft(==>>.empty[A, IndexedSet[Edge[A, W]]]) { (map, edge) =>
        val tempMap =
            if (edge.from === edge.to)
              map
            else if (map.member(edge.from))
              map.insert(edge.from, IndexedSet.singleton(edge))
            else
              map.adjust(edge.from, _.insert(edge))

        if (edge.from === edge.to)
          map
        else if (tempMap.member(edge.to))
          tempMap.insert(edge.to, IndexedSet.singleton(edge.reverse))
        else
          tempMap.adjust(edge.to, _.insert(edge.reverse))
      }
    new Graph(adjacencyList, false)
  }
}
