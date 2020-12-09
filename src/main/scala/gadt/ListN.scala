package gadt

import gadt.Basis._

object ListN {

  trait Cases[T, Result[_, _ <: Nat]] {
    def Nil: Result[T, Zero]
    def Cons[N <: Nat](v: T, vs: ListN[T, N]): Result[T, Inc[N]]
  }

  trait ListN[T, N <: Nat] {
    def on[Result[_, _ <: Nat]]: Cases[T, Result] => Result[T, N]
  }

  def ListN[T] =
    new Cases[T, ListN] {
      def Nil =
        new ListN[T, Zero] { def on[Result[_, _ <: Nat]] = _.Nil }
      def Cons[N <: Nat](v: T, vs: ListN[T, N]) =
        new ListN[T, Inc[N]] { def on[Result[_, _ <: Nat]] = _.Cons(v, vs) }
    }

  val n0: ListN[Int, Zero] = ListN[Int].Nil
  val n1: ListN[Int, Inc[Zero]] = ListN[Int].Cons(2, n0)
  val n2: ListN[Int, Inc[Inc[Zero]]] = ListN[Int].Cons[Inc[Zero]](4, n1)

  def head[T, N <: Nat](nonEmpty: ListN[T, Inc[N]]) =
    nonEmpty on new Cases[T, T1Of2] {
      def Nil = ???
      def Cons[N <: Nat](v: T, vs: ListN[T, N]) = v
    }

}
