package nets

import scala.collection.immutable.Seq

import scalaz.{ ==>>, Order }

final class Graph[A] private(private val adjacencyList: A ==>> IndexedSet[A], val isDirected: Boolean) {
  def nodes: List[A] = adjacencyList.keys
  def vertices: List[A] = nodes
  def degree(node: A)(implicit A: Order[A]): Option[Int] = adjacencyList.lookup(node).map(_.size)

  def edges: List[(A, A)] = 
    adjacencyList.toList.flatMap {
      case (u, ns) => ns.foldLeft(List.empty[(A, A)])((l, v) => (u, v) :: l)
    }

  def size: Int = {
    val bidirectionalEdges = adjacencyList.fold(0)((_, ns, acc) => acc + ns.size)
    if (isDirected) bidirectionalEdges else bidirectionalEdges / 2
  }

  def order: Int = adjacencyList.size

  val isUndirected: Boolean = !isDirected
}

object Graph {
  def fromDirectedEdges[A : Order](es: Seq[(A, A)]): Graph[A] = {
    val adjacencyList =
      es.foldLeft(==>>.empty[A, IndexedSet[A]]) { (map, edge) =>
        val (u, v) = edge
        if (u == v) map
        else if (map.member(u)) map.insert(u, IndexedSet.singleton(v))
        else map.adjust(u, _.insert(v))
      }
    new Graph(adjacencyList, true)
  }

  def fromUndirectedEdges[A : Order](es: Seq[(A, A)]): Graph[A] = {
    val adjacencyList =
      es.foldLeft(==>>.empty[A, IndexedSet[A]]) { (map, edge) =>
        val (u, v) = edge
        val tempMap = if (u == v) map
                      else if (map.member(u)) map.insert(u, IndexedSet.singleton(v))
                      else map.adjust(u, _.insert(v))

        if (u == v) map
        else if (tempMap.member(v)) tempMap.insert(v, IndexedSet.singleton(u))
        else tempMap.adjust(v, _.insert(u))
      }
    new Graph(adjacencyList, false)
  }
}
