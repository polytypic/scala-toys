package fio.fiber

import fio.core._
import fio.comm._

trait FIO[-Env, +Err, +Res] {
  def doIO(wr: Worker, env: Env, cont: Cont[Err, Res]): Unit

  final def flatMap[SubEnv <: Env, SupErr >: Err, ExtRes](
    fn: Res => FIO[SubEnv, SupErr, ExtRes]
  ) = FIO.flatMap(fn)(this)

  final def map[ExtRes](fn: Res => ExtRes) = FIO.map(fn)(this)

  final def fork = FIO.fork(this)
}

object FIO {
  def runOn[Env, Err, Res](sr: Scheduler)(env: Env)(fio: FIO[Env, Err, Res]) = {
    val ivar = asIVar(fio, env)
    Scheduler.pushNew(sr, ivar)
    IVar.await(ivar)
  }

  def succeed[Res](res: Res) = new FIO[Any, Nothing, Res] {
    def doIO(wr: Worker, env: Any, cont: Cont[Nothing, Res]) =
      cont.doRes(wr, res)
  }

  def environment[Env] = new FIO[Env, Nothing, Env] {
    def doIO(wr: Worker, env: Env, cont: Cont[Nothing, Env]) =
      cont.doRes(wr, env)
  }

  def flatMap[Env, Err, Res, SubEnv <: Env, SupErr >: Err, ExtRes](
    fn: Res => FIO[SubEnv, SupErr, ExtRes]
  )(fio: FIO[Env, Err, Res]) =
    new FIO[SubEnv, SupErr, ExtRes] {
      def doIO(wr: Worker, env: SubEnv, cont: Cont[SupErr, ExtRes]) =
        fio.doIO(
          wr,
          env,
          new Awaiter[Err, Res] {
            def doRes(wr: Worker, res: Res) = fn(res).doIO(wr, env, cont)
            def doErr(wr: Worker, err: Err) = cont.doErr(wr, err)
          }
        )
    }

  def map[Env, Err, Res, ExtRes](fn: Res => ExtRes)(fio: FIO[Env, Err, Res]) =
    new FIO[Env, Err, ExtRes] {
      def doIO(wr: Worker, env: Env, cont: Cont[Err, ExtRes]) =
        fio.doIO(wr, env, new Awaiter[Err, Res] {
          def doRes(wr: Worker, res: Res) = cont.doRes(wr, fn(res))
          def doErr(wr: Worker, err: Err) = cont.doErr(wr, err)
        })
    }

  def fork[Env, Err, Res](fio: FIO[Env, Err, Res]) =
    new FIO[Env, Nothing, FIO[Any, Err, Res]] {
      def doIO(
        wr: Worker,
        env: Env,
        cont: Cont[Nothing, FIO[Any, Err, Res]]
      ) = {
        val ivar = asIVar(fio, env)
        Worker.pushNew(wr, ivar)
        cont.doRes(wr, ivar)
      }
    }

  def asIVar[Env, Err, Res](fioIn: FIO[Env, Err, Res], envIn: Env) = {
    var fioMut = fioIn
    var envMut = envIn
    new IVar[Err, Res] {
      override def doWork(wr: Worker) = {
        val fio = fioMut
        if (null != fio) {
          fioMut = null
          val env = envMut
          envMut = null.asInstanceOf[Env]
          fio.doIO(wr, env, this)
        }
      }
    }
  }
}
