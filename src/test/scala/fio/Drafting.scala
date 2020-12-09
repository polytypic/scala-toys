package test.fio

import fio.fiber.FIO._

object Drafting {

  val foo = for {
    x <- succeed(101)
    e <- environment[Long]
  } yield (x + e)

}
