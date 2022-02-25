package it.unibo.casestudy

object DesUtils {
  def consume(des: DesIncarnation.DesSimulator): Unit =
    LazyList.continually(des.fire()).takeWhile(_.nonEmpty).foreach { _ => }
}
