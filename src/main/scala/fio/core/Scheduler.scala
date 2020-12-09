package fio.core

final class Scheduler {
  @volatile var workCount: Long = 0
  var workStack: Work = null
}

object Scheduler {
  def start(sr: Scheduler) =
    for (i <- 0 until Runtime.getRuntime().availableProcessors()) {
      val wrt = new Thread {
        override def run = {
          val wr = new Worker(i, sr)
          Worker.run(wr)
        }
      }
      wrt.setDaemon(true)
      wrt.start()
    }

  def pushAll(sr: Scheduler, work: Work) = {
    var n = 1L
    var tail = work
    while (null != tail.next) {
      tail = tail.next
      n += 1
    }

    pushN(sr, n, work, tail)
  }

  def pushNew(sr: Scheduler, work: Work) =
    pushN(sr, 1L, work, work)

  def pushN(sr: Scheduler, n: Long, work: Work, tail: Work) =
    sr.synchronized {
      tail.next = sr.workStack
      sr.workStack = work
      val nWas = sr.workCount
      sr.workCount = nWas + n
      if (0 == nWas) {
        sr.notify()
      }
    }

  def popSome(sr: Scheduler, wr: Worker): Boolean = {
    var work: Work = null

    sr.synchronized {
      while (0L == sr.workCount) {
        sr.wait()
      }

      work = sr.workStack
      if (null != work) {
        sr.workStack = work.next
        val n = sr.workCount - 1
        sr.workCount = n
        if (0 != n) {
          sr.notify()
        }
      }
    }

    if (null != work) {
      wr.workStack = work
      work.next = null
      true
    } else {
      false
    }
  }
}
