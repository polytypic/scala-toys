package gadt

object StaticSum {
  trait Cases[L, R, Result[_]] {
    def Inl(value: L): Result[L]
    def Inr(value: R): Result[R]
  }

  trait StaticSum[L, R, X] {
    def on[Result[_]]: Cases[L, R, Result] => Result[X]
  }

  def StaticSum[L, R] = {
    type F[X] = StaticSum[L, R, X]
    new Cases[L, R, F] {
      def Inl(value: L) = new F[L] { def on[Result[_]] = _.Inl(value) }
      def Inr(value: R) = new F[R] { def on[Result[_]] = _.Inr(value) }
    }
  }
}
