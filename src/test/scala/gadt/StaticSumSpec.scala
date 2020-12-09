package gadt

import gadt.StaticSum._
import gadt.Basis._

object StaticSumSpec {

  val lInt = StaticSum[Int, Float].Inl(101)
  val rFloat = StaticSum[Int, Float].Inr(4.2f)

  def out[L, R, X](sum: StaticSum[L, R, X]) =
    sum on new Cases[L, R, Id] {
      def Inl(value: L) = value
      def Inr(value: R) = value
    }

  val anInt: Int = out(lInt)
  val aFloat: Float = out(rFloat)

  val x = Left(1)
}
