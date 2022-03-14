package it.unibo.casestudy.utils

import it.unibo.casestudy.DesIncarnation

object DesUtils {
  /** Progress the simulation continuously, it could be an infinite loop so pay attention..
    * @param des
    *   the simulator used to consume all the events
    */
  def consume(des: DesIncarnation.DesSimulator): Unit =
    LazyList.continually(des.fire()).takeWhile(_.nonEmpty).foreach { _ => }
}
