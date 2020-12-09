package fio.core

abstract class Work {
  var next: Work = null
  def doWork(wr: Worker): Unit
}
