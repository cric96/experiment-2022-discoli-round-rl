package it.unibo.casestudy.utils

import com.github.tototoshi.csv.CSVWriter

import java.time.Instant
import scala.collection.immutable.Queue

class ExperimentTrace[T](val name: String) {
  var values: Queue[(Instant, T)] = Queue.empty
  def record(time: Instant, value: T): Unit =
    values = values.appended((time, value))
}

object ExperimentTrace {
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
