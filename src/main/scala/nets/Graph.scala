package nets

import scala.collection.immutable.{ Queue, Seq }

import scalaz.{ ==>>, Order }
import scalaz.syntax.order._

import spire.algebra.{ Rig }

final class Graph[A, W] private(
    private val adjList: A ==>> IndexedSet[Edge[A, W]],
    val isDirected: Boolean) {

  /* Vertex-centric functions */
  def degree(u: A)(implicit A: Order[A]): Option[Int] =
    adjList.lookup(u).map(_.size)
  def neighbors(u: A)(implicit A: Order[A]): Option[IndexedSet[Edge[A, W]]] =
    adjList.lookup(u)
  def nodes: List[A] = vertices
  def order: Int = adjList.size
  def vertices: List[A] = adjList.keys

  /* Edge-centric functions */
  def edges: List[Edge[A, W]] = adjList.values.flatMap(_.toList)

  def size: Int = {
    val bidirectionalEdges = adjList.fold(0)((_, ns, acc) => acc + ns.size)
    if (isDirected) bidirectionalEdges else bidirectionalEdges / 2
  }

  def bfs(root: A)(implicit A: Order[A]): Option[A ==>> Int] = {
    def bfsStep(map: A ==>> Int, queue: Queue[A], nbors: IndexedSet[Edge[A, W]], value: Int): (A ==>> Int, Queue[A]) =
      nbors.foldRight((map, queue)) {
        case (nbor, p@(m, q)) =>
          if (map.member(nbor.to)) p
          else (m.insert(nbor.to, value), q.enqueue(nbor.to))
      }

    def bfsAux(map: A ==>> Int, queue: Queue[A]): Option[A ==>> Int] =
      if (queue.nonEmpty) {
        val (u, rest) = queue.dequeue
        for {
          length <- map.lookup(u)
          nbors  <- neighbors(u)
          args   =  bfsStep(map, queue, nbors, length + 1)
          m      <- bfsAux(args._1, args._2)
        } yield m
      } else Some(map)
    
    bfsAux(==>>.singleton(root, 0), Queue(root))
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
          if (edge.from === edge.to) map
          else if (map.member(edge.from)) map.insert(edge.from, IndexedSet.singleton(edge))
          else map.adjust(edge.from, _.insert(edge))

        if (edge.from === edge.to) map
        else if (tempMap.member(edge.to)) tempMap.insert(edge.to, IndexedSet.singleton(edge.reverse))
        else tempMap.adjust(edge.to, _.insert(edge.reverse))
      }
    new Graph(adjacencyList, false)
  }
}
