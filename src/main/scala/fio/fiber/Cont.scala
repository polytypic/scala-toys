package fio.fiber

import fio.core._

abstract class Cont[-Err, -Res] extends Work {
  def setRes(wr: Worker, res: Res): Unit
  def setErr(wr: Worker, err: Err): Unit
  def doRes(wr: Worker, res: Res): Unit
  def doErr(wr: Worker, err: Err): Unit
}

object Cont {
  def dispatchRes[Err, Res](wr: Worker, res: Res, readers: Cont[Err, Res]) =
    if (null != readers) {
      var last: Work = null
      var next: Work = readers
      do {
        last = next
        next = next.next
        last.asInstanceOf[Cont[Err, Res]].setRes(wr, res)
      } while (null != next)
      Worker.push(wr, readers, last)
    }

  def dispatchErr[Err, Res](wr: Worker, err: Err, readers: Cont[Err, Res]) =
    if (null != readers) {
      var last: Work = null
      var next: Work = readers
      do {
        last = next
        next = next.next
        last.asInstanceOf[Cont[Err, Res]].setErr(wr, err)
      } while (null != next)
      Worker.push(wr, readers, last)
    }
}
