package gadt

object Type {
  trait Cases[Result[_]] {
    def Unit: Result[Unit]
    def Boolean: Result[Boolean]
    def Float: Result[Float]
    def Double: Result[Double]
    def Byte: Result[Byte]
    def Short: Result[Short]
    def Int: Result[Int]
    def Long: Result[Long]
    def Char: Result[Char]
    def String: Result[String]
    def Either[L, R](typeL: Type[L], typeR: Type[R]): Result[Either[L, R]]
    def Tuple[T1, T2](type1: Type[T1], type2: Type[T2]): Result[(T1, T2)]
    def Fun[D, C](typeD: Type[D], typeC: Type[C]): Result[D => C]
    def iso[A, B](typeA: Type[A], toB: A => B, toA: B => A): Result[B]
    def delay[A](typeA: => Type[A]): Result[A]
  }

  trait Type[T] { def on[Result[_]]: Cases[Result] => Result[T] }

  object Type extends Cases[Type] {
    val Unit = new Type[Unit] { def on[Result[_]] = _.Unit }
    val Boolean = new Type[Boolean] { def on[Result[_]] = _.Boolean }
    val Float = new Type[Float] { def on[Result[_]] = _.Float }
    val Double = new Type[Double] { def on[Result[_]] = _.Double }
    val Byte = new Type[Byte] { def on[Result[_]] = _.Byte }
    val Short = new Type[Short] { def on[Result[_]] = _.Short }
    val Int = new Type[Int] { def on[Result[_]] = _.Int }
    val Long = new Type[Long] { def on[Result[_]] = _.Long }
    val Char = new Type[Char] { def on[Result[_]] = _.Char }
    val String = new Type[String] { def on[Result[_]] = _.String }
    def Either[L, R](typeL: Type[L], typeR: Type[R]) = new Type[Either[L, R]] {
      def on[Result[_]] = _.Either(typeL, typeR)
    }
    def Tuple[T1, T2](type1: Type[T1], type2: Type[T2]) = new Type[(T1, T2)] {
      def on[Result[_]] = _.Tuple(type1, type2)
    }
    def Fun[D, C](typeD: Type[D], typeC: Type[C]) = new Type[D => C] {
      def on[Result[_]] = _.Fun(typeD, typeC)
    }
    def iso[A, B](typeA: Type[A], toB: A => B, toA: B => A) = new Type[B] {
      def on[Result[_]] = _.iso(typeA, toB, toA)
    }
    def delay[A](typeA: => Type[A]) = new Type[A] {
      def on[Result[_]] = _.delay(typeA)
    }
  }
}
