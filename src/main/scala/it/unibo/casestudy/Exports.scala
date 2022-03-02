package it.unibo.casestudy
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentTrace

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
object Exports {
  case class NumericValueExport[T: Numeric](
      when: Instant,
      each: FiniteDuration,
      extract: CONTEXT => T,
      name: String
  )(val trace: ExperimentTrace[T] = new ExperimentTrace[T](name))
      extends Event {

    override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
      val exportData = network.ids.toList
        .map(network.context)
        .map(c => extract(c))
        .filter(Numeric[T].toDouble(_).isFinite)
        .sum
      trace.record(when, exportData)
      Some(this.copy(when = when.plusMillis(each.toMillis))(this.trace))
    }
    override val priority: DesIncarnation.Priority = High
  }
  object NumericValueExport {
    def fromSensor[T: Numeric](when: Instant, each: FiniteDuration, name: String): NumericValueExport[T] =
      NumericValueExport(when, each, c => c.sense[T](name).get, name)()

    def export[T: Numeric](when: Instant, each: FiniteDuration): NumericValueExport[T] =
      NumericValueExport(
        when,
        each,
        context =>
          context.exports().filter(_._1 == context.selfId).map(_._2.root[T]()).headOption.getOrElse(Numeric[T].zero),
        "export"
      )()
  }
}
