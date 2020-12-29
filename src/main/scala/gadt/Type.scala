package gadt

sealed trait Type[T]
final case class UnitT() extends Type[Unit]
final case class BooleanT() extends Type[Boolean]
final case class FloatT() extends Type[Float]
final case class DoubleT() extends Type[Double]
final case class ByteT() extends Type[Byte]
final case class ShortT() extends Type[Short]
final case class IntT() extends Type[Int]
final case class LongT() extends Type[Long]
final case class CharT() extends Type[Char]
final case class StringT() extends Type[String]
final case class EitherT[L, R](typeL: Type[L], typeR: Type[R])
    extends Type[Either[L, R]]
final case class TupleT[T1, T2](type1: Type[T1], type2: Type[T2])
    extends Type[(T1, T2)]
final case class FunT[D, C](typeD: Type[D], typeC: Type[C]) extends Type[D => C]
final case class IsoT[A, B](typeA: Type[A], toB: A => B, toA: B => A)
    extends Type[B]
final case class DelayT[A](typeA: () => Type[A]) extends Type[A]
