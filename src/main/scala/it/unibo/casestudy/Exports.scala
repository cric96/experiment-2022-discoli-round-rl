package it.unibo.casestudy
import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentTrace

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** Utility used to export data from a DES simulation */
object Exports {
  /** Export the sensor of name "name" at each "each" starting from "when". Then, it aggregates the values with the
    * "aggregator" function
    * @param when
    *   the initial time in which the export is evaluated
    * @param each
    *   the period in which the export is run
    * @param extract
    *   how to extract data from the context
    * @param aggregator
    *   how to combine the data of the entire system
    * @param name
    *   the name related with the export
    * @param trace
    *   the place in which the data in recorded
    * @tparam T
    *   the type extracted from the context
    * @tparam Z
    *   the type produced after the aggregation
    */
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
    /// Factories

    /** Create an export that observes a particular sensor
      * @param when
      *   the initial time in which the export is evaluated
      * @param each
      *   the period in which the export is run
      * @param name
      *   the name related with the export
      * @tparam T
      *   the type extracted from the context
      * @return
      *   the exported created that observe a particular sensor name
      */
    def fromSensor[T: Numeric](when: Instant, each: FiniteDuration, name: String): NumericValueExport[T, Map[ID, T]] =
      NumericValueExport(when, each, c => c.sense[T](name).get, identity[T], name)()

    /** Create an export that observes the export data of nodes
      * @param when
      *   the initial time in which the export is evaluated
      * @param each
      *   the period in which the export is run
      * @tparam T
      *   the type extracted from the context
      * @return
      *   the exported created that observes a particular sensor name
      */
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
