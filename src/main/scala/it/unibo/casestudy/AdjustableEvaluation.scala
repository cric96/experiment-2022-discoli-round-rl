package it.unibo.casestudy

import it.unibo.casestudy.DesIncarnation._

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class AdjustableEvaluation(node: ID, program: EXECUTION,
                                when: Instant, dt: FiniteDuration,
                                maxDt: FiniteDuration,
                                startWith: FiniteDuration,
                                val localData: Option[EXPORT] = None) extends RoundEvent {
  override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
    network.progress(node, program)
    val netExport: Option[EXPORT] = network.`export`(node)
    val nextFire = (localData, netExport) match {
      case (Some(prev), Some(curr)) if (prev.root() == curr.root()) && (dt + dt < maxDt) => dt + dt
      case (Some(prev), Some(curr)) if (prev.root() != curr.root()) => startWith
      case _ => dt
    }
    val context = network.context(node)
    network.chgSensorValue(ExperimentConstant.RoundCount, Set(node), context.sense[Int](ExperimentConstant.RoundCount).get + 1)
    Option(this.copy(when = when.plusMillis(dt.toMillis), dt = nextFire, localData = netExport))
  }
}