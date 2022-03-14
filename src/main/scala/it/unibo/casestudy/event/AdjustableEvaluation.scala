package it.unibo.casestudy.event

import it.unibo.casestudy.DesIncarnation.{EXECUTION, EXPORT, ID}
import it.unibo.casestudy.DesIncarnation
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentConstant

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** Ad adjustable round frequency that tries to reduce the power consumption as much as possible. For doing this, it
  * reduce the frequency when the current output is less or equals to the previous one.
  * @param node
  *   the target
  * @param program
  *   the program that should be executed
  * @param when
  *   the initial fire moment
  * @param dt
  *   the period
  * @param maxDt
  *   the maxiumum sleep time
  * @param startWith
  *   the initial delta time
  * @param localData
  *   the perivious export data
  */
case class AdjustableEvaluation(
    node: ID,
    program: EXECUTION,
    when: Instant,
    dt: FiniteDuration,
    maxDt: FiniteDuration,
    startWith: FiniteDuration,
    localData: Option[EXPORT] = None
) extends RoundEvent {
  override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
    network.progress(node, program)
    val netExport: Option[EXPORT] = network.`export`(node)
    val nextFire = (localData, netExport) match {
      case (Some(prev), Some(curr)) if (prev.root() == curr.root()) && (dt + dt < maxDt) => dt + dt
      case (Some(prev), Some(curr)) if prev.root() != curr.root() => startWith
      case _ => dt
    }
    val context = network.context(node)
    network.chgSensorValue(
      ExperimentConstant.RoundCount,
      Set(node),
      context.sense[Int](ExperimentConstant.RoundCount).get + 1
    )
    Option(this.copy(when = when.plusMillis(dt.toMillis), dt = nextFire, localData = netExport))
  }
}
