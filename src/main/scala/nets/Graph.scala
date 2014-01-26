package nets

import scala.collection.immutable.{ Queue, Seq }

import scalaz.{ ==>>, Order }
import scalaz.syntax.monoid._
import scalaz.syntax.order._

import spire.algebra.{ Rig }

final class Graph[A, W] private(
    private val adjList: A ==>> IndexedSet[Edge[A, W]],
    val isDirected: Boolean) {

  /* Vertex-centric functions */
  def addVertex(u: A)(implicit A: Order[A]): Graph[A, W] =
    new Graph(adjList.insert(u, IndexedSet.empty[Edge[A, W]]), isDirected)

  def addVertexIfMissing(u: A)(implicit A: Order[A]): Graph[A, W] =
    if (adjList.member(u)) this else addVertex(u)

  def degree(u: A)(implicit A: Order[A]): Option[Int] =
    adjList.lookup(u).map(_.size)

  def hasVertex(u: A)(implicit A: Order[A]): Boolean = memberVertex(u)

  def neighbors(u: A)(implicit A: Order[A]): Option[IndexedSet[Edge[A, W]]] =
    adjList.lookup(u)

  def memberVertex(u: A)(implicit A: Order[A]): Boolean = adjList.member(u)

  def order: Int = adjList.size

  def vertices: List[A] = adjList.keys

  /* Edge-centric functions */
  def addEdge(e: Edge[A, W])(implicit A: Order[A], W: Rig[W]): Graph[A, W] = {
    val oneWay = ==>>.singleton(e.from, IndexedSet.singleton(e))
    val toInsert = if (isDirected) oneWay else oneWay.insert(e.to, IndexedSet.singleton(e.reverse))
    new Graph(adjList |+| toInsert, isDirected)
  }

  def edges: List[Edge[A, W]] = adjList.values.flatMap(_.toList)

  def hadEdge(e: Edge[A, W])(implicit A: Order[A]): Boolean = memberEdge(e)

  def memberEdge(e: Edge[A, W])(implicit A: Order[A]): Boolean =
    adjList.lookup(e.from).map(_.contains(e)).isEmpty

  def size: Int = {
    val bidirectionalEdges = adjList.fold(0)((_, ns, acc) => acc + ns.size)
    if (isDirected) bidirectionalEdges else bidirectionalEdges / 2
  }

  /* Traversal functions */
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
  def emptyDirected[A : Order, W](vs: Seq[A]): Graph[A, W] =
    vs.foldLeft(nullDirected[A, W])((g, v) => g.addVertex(v))

  def emptyUndirected[A : Order, W](vs: Seq[A]): Graph[A, W] =
    vs.foldLeft(nullUndirected[A, W])((g, v) => g.addVertex(v))

  def nullDirected[A, W]: Graph[A, W] = new Graph(==>>.empty, true)

  def nullUndirected[A, W]: Graph[A, W] = new Graph(==>>.empty, false)

  def fromDirectedEdges[A : Order, W : Rig](es: Seq[Edge[A, W]]): Graph[A, W] =
    es.foldLeft(nullDirected[A, W])((g, e) => g.addEdge(e))

  def fromUndirectedEdges[A : Order, W : Rig](es: Seq[Edge[A, W]]): Graph[A, W] =
    es.foldLeft(nullUndirected[A, W])((g, e) => g.addEdge(e))
}
