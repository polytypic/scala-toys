package gadt

object Hoas {
  sealed trait Expr[T]
  final case class Value[A](value: A) extends Expr[A]
  final case class BinOp[A, B, C](
    binOp: (A, B) => C,
    lhs: Expr[A],
    rhs: Expr[B]
  ) extends Expr[C]
  final case class If[A](
    condition: Expr[Boolean],
    onTrue: Expr[A],
    onFalse: Expr[A]
  ) extends Expr[A]
  final case class Apply[A, B](function: Expr[A => B], argument: Expr[A])
      extends Expr[B]
  final case class Lambda[A, B](lambda: Expr[A] => Expr[B]) extends Expr[A => B]
  final case class Fix[A, B](fix: Expr[(A => B) => A => B]) extends Expr[A => B]

  val fact: Expr[Int => Int] = Fix(
    Lambda(f =>
      Lambda(x =>
        If(
          BinOp((_: Int) == (_: Int), x, Value(0)),
          Value(1),
          BinOp(
            (_: Int) * (_: Int),
            x,
            Apply(f, BinOp((_: Int) - (_: Int), x, Value(1)))
          )
        )
      )
    )
  )

  def eval[T](expr: Expr[T]): T =
    expr match {
      case e: Value[a] => e.value
      case e: BinOp[a, b, c] => e.binOp(eval(e.lhs), eval(e.rhs))
      case e: If[a] => eval(if (eval(e.condition)) e.onTrue else e.onFalse)
      case e: Apply[a, b] => eval(e.function)(eval(e.argument))
      case e: Lambda[a, b] => (argument: a) => eval(e.lambda(Value(argument)))
      case e: Fix[a, b] => {
        val fn = eval(e.fix)
        def rec(x: a): b = fn(rec(_))(x)
        rec(_)
      }
    }

  val one_hundred_twenty: Int = eval(Apply(fact, Value(5)))
}
