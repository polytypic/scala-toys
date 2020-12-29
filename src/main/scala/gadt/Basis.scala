package gadt

object Basis {
  def id[A](x: A) = x

  type Id[T] = T

  type T1Of2[T1, T2] = T1

  sealed trait Nat {}
  final case class Zero() extends Nat
  final case class Inc[N <: Nat]() extends Nat
}
