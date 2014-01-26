package nets

import scalaz.{ Order, Ordering }

import spire.algebra.{ Rig }

final class Edge[A, W] private(val from: A, val to: A, val weight: W) {
  def reverse(implicit W: Rig[W]): Edge[A, W] = Edge.weighted(to, from, weight)
}

object Edge extends EdgeInstances {
  def unweighted[A, W](from: A, to: A)(implicit W: Rig[W]): Edge[A, W] =
    new Edge(from, to, W.one)

  def weighted[A, W](from: A, to: A, weight: W)(implicit W: Rig[W]): Edge[A, W] =
    new Edge(from, to, weight)
}

trait EdgeInstances {
  implicit def edgeInstances[A : Order, W]: Order[Edge[A, W]] =
    new Order[Edge[A, W]] {
      override def order(x: Edge[A, W], y: Edge[A, W]): Ordering =
        Order[A].order(x.to, y.to)
    }
}
