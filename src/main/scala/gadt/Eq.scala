package gadt

import Type._

object Eq {
  type Eq[A] = (A, A) => Boolean

  def eqOf[T](typeT: Type[T]): Eq[T] =
    typeT on new Cases[Eq] {
      val Unit = (_, _) => true
      val Boolean = _ == _
      val Float = _ == _ // TODO: NaN
      val Double = _ == _ // TODO: NaN
      val Byte = _ == _
      val Short = _ == _
      val Int = _ == _
      val Long = _ == _
      val Char = _ == _
      val String = _ == _
      def Either[L, R](typeL: Type[L], typeR: Type[R]) = {
        val eqL = eqOf(typeL)
        val eqR = eqOf(typeR)
        (l, r) =>
          (l, r) match {
            case (Left(ll), Left(lr))   => eqL(ll, lr)
            case (Right(rl), Right(rr)) => eqR(rl, rr)
            case _                      => false
          }
      }
      def Tuple[T1, T2](type1: Type[T1], type2: Type[T2]) = {
        val eq1 = eqOf(type1)
        val eq2 = eqOf(type2)
        (l, r) => eq1(l._1, r._1) && eq2(l._2, r._2)
      }
      def Fun[D, C](typeD: Type[D], typeC: Type[C]) = (_, _) => false
      def iso[A, B](typeA: Type[A], toB: A => B, toA: B => A) = {
        val eqA = eqOf(typeA)
        (l, r) => eqA(toA(l), toA(r))
      }
      def delay[A](typeA: => Type[A]) = {
        lazy val eqA = eqOf(typeA)
        (l, r) => eqA(l, r)
      }
    }

  def eq[T](lhs: T, rhs: T)(implicit typeT: Type[T]): Boolean =
    eqOf(typeT)(lhs, rhs)
}
