package gadt

import gadt.Basis._

sealed trait ListN[T, N <: Nat]
final case class Nil[T]() extends ListN[T, Zero]
final case class Cons[T, N <: Nat](head: T, tail: ListN[T, N])
    extends ListN[T, Inc[N]]

object ListN {
  val n0: ListN[Int, Zero] = Nil()
  val n1: ListN[Int, Inc[Zero]] = Cons(2, n0)
  val n2: ListN[Int, Inc[Inc[Zero]]] = Cons(4, n1)

  def head[T, N <: Nat](nonEmpty: ListN[T, Inc[N]]) =
    nonEmpty match {
      case l: Cons[t, n] => l.head
    }
}
