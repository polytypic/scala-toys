package fio.fiber

import fio.core._

abstract class Awaiter[Err, Res] extends Cont[Err, Res] {
  private var isErr = false
  private var res: Res = null.asInstanceOf[Res]
  private var err: Err = null.asInstanceOf[Err]
  final def setRes(wr: Worker, res: Res) = this.res = res
  final def setErr(wr: Worker, err: Err) = { this.isErr = true; this.err = err }
  final def doWork(wr: Worker) = if (isErr) doErr(wr, err) else doRes(wr, res)
}
