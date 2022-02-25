package it.unibo.casestudy
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentTrace

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
object Exports {
  case class NumericValueExport[T: Numeric](
      when: Instant,
      each: FiniteDuration,
      name: String,
      trace: ExperimentTrace[T] = new ExperimentTrace[T]()
  ) extends Event {
    override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
      val exportData = network.ids.toList
        .map(network.context)
        .map(_.sense[T](name).get)
        .sum
      trace.record(when, exportData)
      Some(this.copy(when = when.plusMillis(each.toMillis)))
    }
    override val priority: DesIncarnation.Priority = High
  }
}
