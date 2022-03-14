package it.unibo.casestudy.utils

import com.github.tototoshi.csv.CSVWriter

import java.time.Instant
import scala.collection.immutable.Queue

/** A trace of a particular data produce by an experiment
  * @param name
  *   the trace name
  * @tparam T
  *   the data type
  */
class ExperimentTrace[T](val name: String) {
  /** The data recorded */
  var values: Queue[(Instant, T)] = Queue.empty

  /** Append a new value to the local data.
    * @param time
    *   when the data is produced
    * @param value
    *   the value to store
    */
  def record(time: Instant, value: T): Unit =
    values = values.appended((time, value))

  /** Utility used to covert the inner data from a representation to another.
    * @param fun
    *   the mapping function
    * @tparam Z
    *   the output type
    * @return
    */
  def convert[Z](fun: (Instant, T) => Z): ExperimentTrace[Z] = {
    val newTrace = new ExperimentTrace[Z](name)
    newTrace.values = values.map { case (i, data) => i -> fun(i, data) }
    newTrace
  }
}

object ExperimentTrace {
  /** Utility to store data in csv. The data are printed with to string.
    * @param path
    *   where the data will be stored
    * @param experimentTrace
    *   the trace to store
    */
  def storeInCsv(path: os.Path, experimentTrace: ExperimentTrace[_]*): Unit = {
    val writer = CSVWriter.open(path.toIO)
    val columns = "time" +: experimentTrace.map(_.name)
    val values = experimentTrace
      .flatMap(_.values)
      .groupMap { case (instant, _) => instant.toEpochMilli } { case (_, value) => value }
      .toSeq
      .sortBy(_._1)
      .map { case (k, v) => k +: v }
    writer.writeAll(columns +: values)
    writer.close()
  }
}
