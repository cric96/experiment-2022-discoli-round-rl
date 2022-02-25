package it.unibo.casestudy
import DesIncarnation._

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
object Exports {
  case class ExportCountEvent(when: Instant, each: FiniteDuration) extends Event {
    override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
      val fireCount = network.ids.toList
        .map(network.context)
        .map(_.sense[Int](ExperimentConstant.RoundCount).get)
        .sum
      println(fireCount)
      Some(this.copy(when = when.plusMillis(each.toMillis)))
    }

    override val priority: DesIncarnation.Priority = High
  }
}
