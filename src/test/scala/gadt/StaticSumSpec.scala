package gadt

import gadt._
import gadt.Basis._

object StaticSumSpec {

  val lInt = Inl(101)
  val rFloat = Inr(4.2f)

  def out[L, R, X](sum: StaticSum[L, R, X]): X =
    sum match {
      case s: Inl[l, r] => s.value
      case s: Inr[l, r] => s.value
    }

  val anInt: Int = out(lInt)
  val aFloat: Float = out(rFloat)

  val x = Left(1)
}
