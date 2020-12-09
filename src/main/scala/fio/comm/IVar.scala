package fio.comm

import fio.core._
import fio.fiber._

class IVar[Err, Res] extends Cont[Err, Res] with FIO[Any, Err, Res] {
  @volatile private var state = 0
  @volatile private var res: Res = null.asInstanceOf[Res]
  @volatile private var err: Err = null.asInstanceOf[Err]
  private var readers: Cont[Err, Res] = null

  final def doIO(wr: Worker, env: Any, cont: Cont[Err, Res]) =
    IVar.doIO(this, wr, cont)

  final def setRes(wr: Worker, res: Res) = IVar.doRes(wr, this, res)
  final def setErr(wr: Worker, err: Err) = IVar.doErr(wr, this, err)
  final def doRes(wr: Worker, res: Res) = IVar.doRes(wr, this, res)
  final def doErr(wr: Worker, err: Err) = IVar.doErr(wr, this, err)

  def doWork(wr: Worker) = {}
}

object IVar {
  def await[Err, Res](ivar: IVar[Err, Res]) = {
    var state = ivar.state
    if (0 == state) ivar.synchronized {
      while (0 == ivar.state) {
        ivar.wait()
      }
      state = ivar.state
    }
    if (state < 0) Left(ivar.err) else Right(ivar.res)
  }

  def setRes[Err, Res](ivar: IVar[Err, Res], res: Res) =
    new FIO[Any, Nothing, Unit] {
      def doIO(wr: Worker, env: Any, cont: Cont[Nothing, Unit]) =
        IVar.doRes(wr, ivar, res)
    }

  def setErr[Err, Res](ivar: IVar[Err, Res], err: Err) =
    new FIO[Any, Nothing, Unit] {
      def doIO(wr: Worker, env: Any, cont: Cont[Nothing, Unit]) =
        IVar.doErr(wr, ivar, err)
    }

  def doIO[Err, Res](ivar: IVar[Err, Res], wr: Worker, cont: Cont[Err, Res]) = {
    var state = ivar.state
    if (0 == state) ivar.synchronized {
      state = ivar.state
      if (0 == state) {
        cont.next = ivar.readers
        ivar.readers = cont
      }
    }
    if (0 != state) {
      if (state < 0) cont.doErr(wr, ivar.err) else cont.doRes(wr, ivar.res)
    }
  }

  def doRes[Err, Res](wr: Worker, ivar: IVar[Err, Res], res: Res) = {
    var got = 0 == ivar.state
    if (got) {
      ivar.synchronized {
        got = 0 == ivar.state
        if (got) {
          ivar.res = res
          ivar.state = 1
          ivar.notifyAll()
        }
      }
      if (got) {
        val readers = ivar.readers
        ivar.readers = null
        Cont.dispatchRes(wr, res, readers)
      }
    }
  }

  def doErr[Err, Res](wr: Worker, ivar: IVar[Err, Res], err: Err) = {
    var got = 0 == ivar.state
    if (got) {
      ivar.synchronized {
        got = 0 == ivar.state
        if (got) {
          ivar.err = err
          ivar.state = -1
          ivar.notifyAll()
        }
      }
      if (got) {
        val readers = ivar.readers
        ivar.readers = null
        Cont.dispatchErr(wr, err, readers)
      }
    }
  }
}
