package it.unibo.casestudy
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentTrace

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
object Exports {
  case class NumericValueExport[T, Z](
      when: Instant,
      each: FiniteDuration,
      extract: CONTEXT => T,
      aggregator: Map[ID, T] => Z,
      name: String
  )(val trace: ExperimentTrace[Z] = new ExperimentTrace[Z](name))
      extends Event {

    override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
      val exportData = network.ids.toList
        .map(id => id -> network.context(id))
        .map { case (id, context) => id -> extract(context) }
        .toMap
      trace.record(when, aggregator(exportData))
      Some(this.copy(when = when.plusMillis(each.toMillis))(this.trace))
    }
    override val priority: DesIncarnation.Priority = High
  }

  object NumericValueExport {
    def identity[T]: Map[ID, T] => Map[ID, T] = a => a
    def fromSensor[T: Numeric](when: Instant, each: FiniteDuration, name: String): NumericValueExport[T, Map[ID, T]] =
      NumericValueExport(when, each, c => c.sense[T](name).get, identity[T], name)()

    def export[T: Numeric](when: Instant, each: FiniteDuration): NumericValueExport[T, Map[ID, T]] =
      NumericValueExport(
        when,
        each,
        context =>
          context.exports().filter(_._1 == context.selfId).map(_._2.root[T]()).headOption.getOrElse(Numeric[T].zero),
        identity[T],
        "export"
      )()
  }
}
