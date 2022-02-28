package it.unibo.casestudy.utils

import it.unibo.casestudy.DesIncarnation

object DesUtils {
  def consume(des: DesIncarnation.DesSimulator): Unit =
    LazyList.continually(des.fire()).takeWhile(_.nonEmpty).foreach { _ => }
}
