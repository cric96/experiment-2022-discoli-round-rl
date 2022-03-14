package it.unibo.casestudy.event

import it.unibo.casestudy.DesIncarnation.ID
import it.unibo.casestudy.DesIncarnation
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentConstant

import java.time.Instant

/** Event used to swap source at certain time as the simulation progress
  * @param when
  *   when the swap should happen
  * @param node
  *   the node target
  * @param value
  *   the next value
  */
case class ChangeSourceAt(when: Instant, node: ID, value: Boolean) extends FireAndForget {
  override def actSingle(network: DesIncarnation.NetworkSimulator): Unit =
    network.chgSensorValue(ExperimentConstant.Source, Set(node), value)
}
