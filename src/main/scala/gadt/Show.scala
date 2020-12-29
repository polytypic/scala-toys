package gadt

import Basis._

object Show {
  type Show[A] = A => String

  def showOf[T](typeT: Type[T]): Show[T] =
    typeT match {
      case _: UnitT => _ => "()"
      case _: BooleanT => _.toString()
      case _: FloatT => _.toString() // TODO: Enough precision
      case _: DoubleT => _.toString() // TODO: Enough precision
      case _: ByteT => _.toString()
      case _: ShortT => _.toString()
      case _: IntT => _.toString()
      case _: LongT => _.toString()
      case _: CharT => "\"" + _ + "\"" // TODO: Escape
      case _: StringT => "\"" + _ + "\"" // TODO: Escape
      case t: EitherT[l, r] => {
        val showL = showOf(t.typeL)
        val showR = showOf(t.typeR)
        id {
          case Left(l) => s"Left(${showL(l)})"
          case Right(r) => s"Right(${showR(r)})"
        }
      }
      case t: TupleT[t1, t2] => {
        val show1 = showOf(t.type1)
        val show2 = showOf(t.type2)
        id { case (v1, v2) => s"(${show1(v1)}, ${show2(v2)})" }
      }
      case _: FunT[d, c] => _ => "<Fun>"
      case t: IsoT[a, b] =>
        t.toA andThen showOf(t.typeA)
      case t: DelayT[a] => {
        lazy val showA = showOf(t.typeA())
        a => showA(a)
      }
    }

  def show[T](value: T)(implicit typeT: Type[T]): String = showOf(typeT)(value)
}
