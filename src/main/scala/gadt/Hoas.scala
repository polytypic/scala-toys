package gadt

import Basis._

object Hoas {
  trait Cases[Result[_]] {
    def Value[A](value: A): Result[A]
    def BinOp[A, B, C](
      binOp: (A, B) => C,
      lhs: Expr[A],
      rhs: Expr[B]
    ): Result[C]
    def If[A](
      condition: Expr[Boolean],
      onTrue: Expr[A],
      onFalse: Expr[A]
    ): Result[A]
    def Apply[A, B](function: Expr[A => B], argument: Expr[A]): Result[B]
    def Lambda[A, B](lambda: Expr[A] => Expr[B]): Result[A => B]
    def Fix[A, B](fix: Expr[(A => B) => A => B]): Result[A => B]
  }

  trait Expr[T] {
    def on[Result[_]](handler: Cases[Result]): Result[T]
  }

  object Expr extends Cases[Expr] {
    def Value[A](value: A) = new Expr[A] {
      def on[Result[_]](handler: Cases[Result]) = handler.Value(value)
    }
    def BinOp[A, B, C](binOp: (A, B) => C, lhs: Expr[A], rhs: Expr[B]) =
      new Expr[C] {
        def on[Result[_]](handler: Cases[Result]) =
          handler.BinOp(binOp, lhs, rhs)
      }
    def If[A](
      condition: Expr[Boolean],
      onTrue: Expr[A],
      onFalse: Expr[A]
    ) = new Expr[A] {
      def on[Result[_]](handler: Cases[Result]) =
        handler.If(condition, onTrue, onFalse)
    }
    def Apply[A, B](function: Expr[A => B], argument: Expr[A]) = new Expr[B] {
      def on[Result[_]](handler: Cases[Result]) =
        handler.Apply(function, argument)
    }
    def Lambda[A, B](lambda: Expr[A] => Expr[B]) = new Expr[A => B] {
      def on[Result[_]](handler: Cases[Result]) =
        handler.Lambda(lambda)
    }
    def Fix[A, B](fix: Expr[(A => B) => A => B]) = new Expr[A => B] {
      def on[Result[_]](handler: Cases[Result]) = handler.Fix(fix)
    }
  }

  import Expr._

  val fact: Expr[Int => Int] = Fix(
    Lambda((f: Expr[Int => Int]) =>
      Lambda((x: Expr[Int]) =>
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
    expr on new Cases[Id] {
      def Value[A](value: A) = value
      def BinOp[A, B, C](bin: (A, B) => C, lhs: Expr[A], rhs: Expr[B]) =
        bin(eval(lhs), eval(rhs))
      def If[A](
        condition: Expr[Boolean],
        onTrue: Expr[A],
        onFalse: Expr[A]
      ) = eval(if (eval(condition)) onTrue else onFalse)
      def Apply[A, B](function: Expr[A => B], argument: Expr[A]) =
        eval(function)(eval(argument))
      def Lambda[A, B](lambda: Expr[A] => Expr[B]) =
        (argument: A) => eval(lambda(Expr.Value(argument)))
      def Fix[A, B](fix: Expr[(A => B) => A => B]) = {
        val fn = eval(fix)
        def rec(x: A): B = fn(rec(_))(x)
        rec(_)
      }
    }

  val one_hundred_twenty: Int = eval(Apply(fact, Value(5)))
}
