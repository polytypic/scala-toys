package gadt

object Optic {
  trait Cases[Result[_, _]] {
    def Elem1[T1, T2]: Result[(T1, T2), T1]
    def Elem2[T1, T2]: Result[(T1, T2), T2]
    def Seq[T]: Result[Seq[T], T]
    def andThen[S, A, B](outer: Optic[S, A], inner: Optic[A, B]): Result[S, B]
  }

  trait Optic[S, A] {
    def on[Result[_, _]]: Cases[Result] => Result[S, A]

    final def andThen[B](inner: Optic[A, B]) = Optic.andThen(this, inner)
  }

  object Optic extends Cases[Optic] {
    def Elem1[T1, T2] =
      new Optic[(T1, T2), T1] { def on[Result[_, _]] = _.Elem1 }
    def Elem2[T1, T2] =
      new Optic[(T1, T2), T2] { def on[Result[_, _]] = _.Elem2 }
    def Seq[T] =
      new Optic[Seq[T], T] { def on[Result[_, _]] = _.Seq }
    def andThen[S, A, B](outer: Optic[S, A], inner: Optic[A, B]) =
      new Optic[S, B] { def on[Result[_, _]] = _.andThen(outer, inner) }
  }

  type First[X, S, A] = (A => Option[X]) => S => Option[X]

  def firstAs[X, S, A](optic: Optic[S, A]): First[X, S, A] = {
    type F[S, A] = First[X, S, A]
    optic on new Cases[F] {
      def Elem1[T1, T2] = f => x => f(x._1)
      def Elem2[T1, T2] = f => x => f(x._2)
      def Seq[T] = f => _.collectFirst(f.unlift)
      def andThen[S, A, B](outer: Optic[S, A], inner: Optic[A, B]) =
        firstAs(inner) andThen firstAs(outer)
    }
  }

  def first[S, A](s: S)(optic: Optic[S, A]) = firstAs(optic)(Some(_))(s)

  type Map[S, A] = (A => A) => S => S

  def map[S, A](optic: Optic[S, A]): Map[S, A] =
    optic on new Cases[Map] {
      def Elem1[T1, T2] = f => x => (f(x._1), x._2)
      def Elem2[T1, T2] = f => x => (x._1, f(x._2))
      def Seq[T] = f => _.map(f)
      def andThen[S, A, B](outer: Optic[S, A], inner: Optic[A, B]) =
        map(inner) andThen map(outer)
    }
}
