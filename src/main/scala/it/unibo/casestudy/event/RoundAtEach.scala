package it.unibo.casestudy.event

import it.unibo.casestudy.DesIncarnation.{EXECUTION, ID}
import it.unibo.casestudy.DesIncarnation
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentConstant

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** Perform the evaluation of a certain aggregate program at each dt period
  * @param node
  *   the target
  * @param program
  *   the program that should be executed
  * @param when
  *   the initial fire moment
  * @param dt
  *   the period
  */
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
