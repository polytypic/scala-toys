package fio.core

final class Worker(val index: Int, val scheduler: Scheduler) {
  var workStack: Work = null
}

object Worker {
  def run(wr: Worker) = {
    val sr = wr.scheduler
    while (Scheduler.popSome(sr, wr)) {
      while (wr.workStack != null) {
        val work = wr.workStack
        var next = work.next
        if (null != next) {
          work.next = null
          if (null == sr.workStack) {
            Scheduler.pushAll(sr, next)
            next = null
          }
        }
        wr.workStack = next

        work.doWork(wr)
      }
    }
  }

  def pushNew(wr: Worker, work: Work) =
    push(wr, work, work)

  def push(wr: Worker, work: Work, last: Work) = {
    val older = wr.workStack
    wr.workStack = work
    if (null != older) {
      val sr = wr.scheduler
      if (0L == sr.workCount) {
        Scheduler.pushAll(sr, older)
      } else {
        last.next = older
      }
    }
  }
}
