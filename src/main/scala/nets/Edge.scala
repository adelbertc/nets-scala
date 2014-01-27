package nets

import scalaz.{ Equal, Monoid, Order, Ordering }
import scalaz.syntax.equal._
import scalaz.syntax.monoid._

import spire.algebra.{ Rig }

final class Edge[A, W] private(val from: A, val to: A, val weight: W) {
  def hasEndpoints(from2: A, to2: A)(implicit A: Equal[A]): Boolean =
    (from === from2) && (to === to2)

  def reverse: Edge[A, W] = new Edge(to, from, weight)
}

object Edge extends EdgeInstances {
  def unweighted[A, W](from: A, to: A)(implicit A: Equal[A], W: Rig[W]): Option[Edge[A, W]] =
    if (from === to) None else Some(new Edge(from, to, W.one))

  def weighted[A, W](from: A, to: A, weight: W)(implicit A: Equal[A]): Option[Edge[A, W]] =
    if (from === to) None else Some(new Edge(from, to, weight))
}

trait EdgeInstances {
  implicit def edgeInstances[A, W](implicit A: Order[A]): Order[Edge[A, W]] =
    new Order[Edge[A, W]] {
      override def order(x: Edge[A, W], y: Edge[A, W]): Ordering =
        A.order(x.from, y.from) |+| A.order(x.to, y.to)
    }
}
