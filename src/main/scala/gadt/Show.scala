package gadt

import Type._

object Show {
  type Show[A] = A => String

  def showOf[T](typeT: Type[T]): Show[T] =
    typeT on new Cases[Show] {
      val Unit = _ => "()"
      val Boolean = _.toString()
      val Float = _.toString() // TODO: Enough precision
      val Double = _.toString() // TODO: Enough precision
      val Byte = _.toString()
      val Short = _.toString()
      val Int = _.toString()
      val Long = _.toString()
      val Char = "\"" + _ + "\"" // TODO: Escape
      val String = "\"" + _ + "\"" // TODO: Escape
      def Either[L, R](typeL: Type[L], typeR: Type[R]) = {
        val showL = showOf(typeL)
        val showR = showOf(typeR)
        _ match {
          case Left(l)  => s"Left(${showL(l)})"
          case Right(r) => s"Right(${showR(r)})"
        }
      }
      def Tuple[T1, T2](type1: Type[T1], type2: Type[T2]) = {
        val show1 = showOf(type1)
        val show2 = showOf(type2)
        _ match { case (v1, v2) => s"(${show1(v1)}, ${show2(v2)})" }
      }
      def Fun[D, C](typeD: Type[D], typeC: Type[C]) = _ => "<Fun>"
      def iso[A, B](typeA: Type[A], toB: A => B, toA: B => A) =
        toA andThen showOf(typeA)
      def delay[A](typeA: => Type[A]) = {
        lazy val showA = showOf(typeA)
        a => showA(a)
      }
    }

  def show[T](value: T)(implicit typeT: Type[T]): String = showOf(typeT)(value)
}
