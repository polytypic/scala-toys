package test.gadt

import gadt.Optic._
import gadt.Optic.Optic._

object OpticSpec {

  def anOptic[A, B, C]: Optic[((A, B), C), B] =
    Elem1 andThen Elem2

  val x = first(((1, true), "scott"))(anOptic)
}
