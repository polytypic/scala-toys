package example

import fio.core._
import fio.fiber._

object Fib {
  var numForks: Long = 0

  def ser(n: Long): Long =
    if (n < 2) n
    else {
      numForks += 1
      ser(n - 2) + ser(n - 1)
    }

  def fibPar(n: Long): FIO[Any, Nothing, Long] =
    if (n < 2) FIO.succeed(n)
    else
      for {
        t2 <- fibPar(n - 2).fork
        f1 <- fibPar(n - 1)
        f2 <- t2
      } yield (f1 + f2)

  def withTime(effect: => Unit) = {
    val start = System.nanoTime()
    effect
    val stop = System.nanoTime()
    stop - start
  }

  def benchOnce(sr: Scheduler, n: Long) = {
    numForks = 0L
    ser(n)
    val num = numForks

    val t = withTime {
      FIO.runOn(sr)(())(fibPar(n)) match {
        case Right(_) => ()
        case Left(_)  => ()
      }
    }
    println(
      s"Fib($n) took ${t / 1000000}ms - ${num} forks -> ${(num.toDouble / 1000000) / (t.toDouble / (1000 * 1000000))} Mforks/s"
    )
  }

  def main(args: Array[String]) = {
    val sr = new Scheduler
    Scheduler.start(sr)

    for (_ <- 0 until 5) {
      for (n <- 20L until 28L)
        benchOnce(sr, n)
      benchOnce(sr, 40)
    }
  }
}
