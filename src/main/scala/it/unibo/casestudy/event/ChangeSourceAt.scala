package it.unibo.casestudy.event

import it.unibo.casestudy.DesIncarnation.ID
import it.unibo.casestudy.{DesIncarnation, ExperimentConstant}
import DesIncarnation._
import java.time.Instant

case class ChangeSourceAt(when: Instant, node: ID, value: Boolean) extends FireAndForget {
  override def actSingle(network: DesIncarnation.NetworkSimulator): Unit =
    network.chgSensorValue(ExperimentConstant.Source, Set(node), value)
}
