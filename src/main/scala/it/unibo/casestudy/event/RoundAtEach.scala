package it.unibo.casestudy.event

import it.unibo.casestudy.DesIncarnation.{EXECUTION, ID}
import it.unibo.casestudy.{DesIncarnation, ExperimentConstant}
import DesIncarnation._
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class RoundAtEach(node: ID, program: EXECUTION, when: Instant, dt: FiniteDuration) extends RoundEvent {
  override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
    network.progress(node, program)
    val context = network.context(node)
    network.chgSensorValue(
      ExperimentConstant.RoundCount,
      Set(node),
      context.sense[Int](ExperimentConstant.RoundCount).get + 1
    )
    Option(this.copy(when = when.plusMillis(dt.toMillis)))
  }
}
