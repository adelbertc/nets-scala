package nets

import java.io.PrintWriter

import scala.collection.immutable.{ Queue, Seq }

import scalaz.{ ==>>, Equal, NonEmptyList, Order, Show }
import scalaz.effect.IO
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.set._
import scalaz.syntax.traverse._
import scalaz.syntax.monoid._
import scalaz.syntax.order._
import scalaz.syntax.show._

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

  def hasVertex(u: A)(implicit A: Order[A]): Boolean = adjList.member(u)

  def neighbors(u: A)(implicit A: Order[A]): Option[IndexedSet[Edge[A, W]]] =
    adjList.lookup(u)

  def order: Int = adjList.size

  def vertexSet: Set[A] = adjList.keySet

  def vertices: List[A] = adjList.keys

  /* Edge-centric functions */
  def addEdge(e: Edge[A, W])(implicit A: Order[A]): Graph[A, W] = {
    val oneWay = ==>>.singleton(e.from, IndexedSet.singleton(e))
    val toInsert = if (isDirected) oneWay else oneWay.insert(e.to, IndexedSet.singleton(e.reverse))
    new Graph(adjList |+| toInsert, isDirected)
  }

  def edges: List[Edge[A, W]] = adjList.values.flatMap(_.toList)

  def getEdge(u: A, v: A)(implicit A: Order[A], W: Rig[W]): Option[Edge[A, W]] =
    for {
      a <- adjList.lookup(u)
      e <- Edge.unweighted[A, W](u, v)
      i <- a.indexOf(e)
      r <- a.elementAt(i)
    } yield r

  def hasEdge(u: A, v: A)(implicit A: Order[A], W: Rig[W]): Boolean =
    getEdge(u, v).nonEmpty

  def size: Int = {
    val bidirectionalEdges = adjList.fold(0)((_, ns, acc) => acc + ns.size)
    if (isDirected) bidirectionalEdges else bidirectionalEdges / 2
  }

  def weight(u: A, v: A)(implicit A: Order[A], W: Rig[W]): Option[W] =
    getEdge(u, v).map(_.weight)

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

  def cost(p: NonEmptyList[(A, A)])(implicit A: Order[A], W: Rig[W]): Option[W] =
    (p.traverseU { case (u, v) => getEdge(u, v) }).flatMap(costE)

  def costE(p: NonEmptyList[Edge[A, W]])(implicit A: Order[A], W: Rig[W]): Option[W] =
    if (hasPath(p.map(e => e.from -> e.to))) {
      Some(p.foldLeft(W.zero)((a, e) => W.plus(a, e.weight)))
    } else None

  def hasPath(p: NonEmptyList[(A, A)])(implicit A: Order[A], W: Rig[W]): Boolean =
    if (hasEdge(p.head._1, p.head._2))
      (p.tail.foldRight(Some(p.head._2): Option[A]) {
        case (ce@(u, v), o) => o.flatMap(p => if (p === u && hasEdge(u, v)) Some(v) else None)
      }).nonEmpty
    else false

  def isConnected(implicit A: Order[A]): Boolean = directed.isStronglyConnected

  def isStronglyConnected(implicit A: Order[A]): Boolean =
    if (order =/= 1) {
      val r = for {
                r <- adjList.keys.headOption
                b <- bfs(r)
              } yield if (b.keySet === vertexSet) true else false
      r.fold(false)(identity)
    } else true

  def isWeaklyConnected(implicit A: Order[A], W: Rig[W]): Boolean = undirected.isConnected

  def undirected(implicit A: Order[A]): Graph[A, W] =
    if (isDirected) {
      val res = edges.map(e => e.to -> IndexedSet.singleton(e.reverse))
      val al = ==>>.fromListWith(res)(_ union _)
      new Graph(adjList |+| al, false)
    } else this

  def directed: Graph[A, W] =
    if (isDirected) this else new Graph(adjList, true)

  val isUndirected: Boolean = !isDirected
}

object Graph extends GraphInstances {
  def emptyDirected[A : Order, W](vs: Seq[A]): Graph[A, W] =
    vs.foldLeft(nullDirected[A, W])((g, v) => g.addVertex(v))

  def emptyUndirected[A : Order, W](vs: Seq[A]): Graph[A, W] =
    vs.foldLeft(nullUndirected[A, W])((g, v) => g.addVertex(v))

  def nullDirected[A, W]: Graph[A, W] = new Graph(==>>.empty, true)

  def nullUndirected[A, W]: Graph[A, W] = new Graph(==>>.empty, false)

  def fromDirectedEdges[A : Order, W : Rig](es: Seq[Edge[A, W]]): Graph[A, W] =
    es.foldLeft(nullDirected[A, W])((g, e) => g.addEdge(e))

  def fromUndirectedEdgesC[A : Order, W : Rig](es: Seq[Edge[A, W]]): Option[Graph[A, W]] = {
    val g = fromUndirectedEdges(es)
    if (g.isConnected) Some(g) else None
  }

  def fromDirectedEdgesSC[A : Order, W : Rig](es: Seq[Edge[A, W]]): Option[Graph[A, W]] = {
    val g = fromDirectedEdges(es)
    if (g.isStronglyConnected) Some(g) else None
  }

  def fromDirectedEdgesWC[A : Order, W : Rig](es: Seq[Edge[A, W]]): Option[Graph[A, W]] = {
    val g = fromDirectedEdges(es)
    if (g.isWeaklyConnected) Some(g) else None
  }

  def fromUndirectedEdges[A : Order, W : Rig](es: Seq[Edge[A, W]]): Graph[A, W] =
    es.foldLeft(nullUndirected[A, W])((g, e) => g.addEdge(e))
}

trait GraphInstances {
  implicit def graphEqual[A : Order, W]: Equal[Graph[A, W]] =
    new Equal[Graph[A, W]] {
      override def equal(a1: Graph[A, W], a2: Graph[A, W]): Boolean =
        (a1.vertices === a2.vertices) && (a1.edges === a2.edges)
    }
}

trait GraphFunctions {
  def writeEdgelist[A : Show, W](graph: Graph[A, W], path: String): IO[Unit] =
    IO {
      val pw = new PrintWriter(path, "UTF-8")
      graph.edges.foreach(e => pw.println(e.from.shows ++ " " ++ e.to.shows))
      pw.close()
    }

  def writeEdgelistW[A : Show, W : Show](graph: Graph[A, W], path: String): IO[Unit] =
    IO {
      val pw = new PrintWriter(path, "UTF-8")
      graph.edges.foreach(e => pw.println(e.from.shows ++ " " ++ e.to.shows ++ " " ++ e.weight.shows))
      pw.close()
    }
}
