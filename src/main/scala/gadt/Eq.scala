package gadt

import Basis._

object Eq {
  type Eq[A] = (A, A) => Boolean

  def eqOf[T](typeT: Type[T]): Eq[T] =
    typeT match {
      case _: UnitT => (_, _) => true
      case _: BooleanT => _ == _
      case _: FloatT => _ == _ // TODO: NaN
      case _: DoubleT => _ == _ // TODO: NaN
      case _: ByteT => _ == _
      case _: ShortT => _ == _
      case _: IntT => _ == _
      case _: LongT => _ == _
      case _: CharT => _ == _
      case _: StringT => _ == _
      case t: EitherT[l, r] => {
        val eqL = eqOf(t.typeL)
        val eqR = eqOf(t.typeR)
        id {
          case (Left(ll), Left(lr)) => eqL(ll, lr)
          case (Right(rl), Right(rr)) => eqR(rl, rr)
          case _ => false
        }
      }
      case t: TupleT[t1, t2] => {
        val eq1 = eqOf(t.type1)
        val eq2 = eqOf(t.type2)
        id { case ((l1, l2), (r1, r2)) => eq1(l1, r1) && eq2(l2, r2) }
      }
      case _: FunT[d, c] => (_, _) => false
      case t: IsoT[a, b] => {
        val eqA = eqOf(t.typeA)
        (l, r) => eqA(t.toA(l), t.toA(r))
      }
      case t: DelayT[a] => {
        lazy val eqA = eqOf(t.typeA())
        (l, r) => eqA(l, r)
      }
    }

  def eq[T](lhs: T, rhs: T)(implicit typeT: Type[T]): Boolean =
    eqOf(typeT)(lhs, rhs)
}
