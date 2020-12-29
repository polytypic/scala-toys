package test.gadt

import gadt._
import gadt.Optic._

object OpticSpec {

  def anOptic[A, B, C]: Optic[((A, B), C), B] =
    (Elem1O()) andThen (Elem2O())

  val x = first(((1, true), "scott"))(anOptic)
}
