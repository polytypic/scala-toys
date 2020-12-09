package gadt

import gadt.Basis._

trait Leibniz[A, B] {
  def apply[F[_]]: F[A] => F[B]
}

object Leibniz {
  def reflexivity[A] = new Leibniz[A, A] { def apply[F[_]] = a => a }

  def transitivity[A, B, C](bc: Leibniz[B, C], ab: Leibniz[A, B]) =
    new Leibniz[A, C] { def apply[F[_]] = ab.apply andThen bc.apply }

  def symmetry[A, B](ab: Leibniz[A, B]) =
    new Leibniz[B, A] {
      def apply[F[_]] = b => {
        type G[X] = F[X] => F[A]
        ab[G](a => a)(b)
      }
    }

  def to[A, B](ab: Leibniz[A, B]) = ab[Id]
  def from[A, B](ab: Leibniz[A, B]) = symmetry(ab)[Id]
}
