package gadt

sealed trait Optic[S, A] {
  final def andThen[B](that: Optic[A, B]) = AndThenO(this, that)
}
final case class Elem1O[T1, T2]() extends Optic[(T1, T2), T1]
final case class Elem2O[T1, T2]() extends Optic[(T1, T2), T2]
final case class SeqO[T]() extends Optic[Seq[T], T]
final case class AndThenO[S, A, B](outer: Optic[S, A], inner: Optic[A, B])
    extends Optic[S, B]

object Optic {
  type First[X, S, A] = (A => Option[X]) => S => Option[X]

  def firstAs[X, S, A](optic: Optic[S, A]): First[X, S, A] = optic match {
    case _: Elem1O[t1, t2] => f => (x: (t1, t2)) => f(x._1)
    case _: Elem2O[t1, t2] => f => (x: (t1, t2)) => f(x._2)
    case _: SeqO[t] => f => (s: Seq[t]) => s.collectFirst(f.unlift)
    case o: AndThenO[s, a, b] => firstAs(o.inner) andThen firstAs(o.outer)
  }

  def first[S, A](s: S)(optic: Optic[S, A]) = firstAs(optic)(Some(_))(s)

  type Map[S, A] = (A => A) => S => S

  def map[S, A](optic: Optic[S, A]): Map[S, A] = optic match {
    case _: Elem1O[t1, t2] => (f: t1 => t1) => (x: (t1, t2)) => (f(x._1), x._2)
    case _: Elem2O[t1, t2] => (f: t2 => t2) => (x: (t1, t2)) => (x._1, f(x._2))
    case _: SeqO[t] => (f: t => t) => (s: Seq[t]) => s.map(f)
    case o: AndThenO[s, a, b] => map(o.inner) andThen map(o.outer)
  }
}
