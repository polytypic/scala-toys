package gadt

sealed trait StaticSum[L, R, X]
final case class Inl[L, R](value: L) extends StaticSum[L, R, L]
final case class Inr[L, R](value: R) extends StaticSum[L, R, R]
