package it.unibo.casestudy.utils

import java.time.Instant
import scala.collection.immutable.Queue

class ExperimentTrace[T](name: String) {
  var values: Queue[(Instant, T)] = Queue.empty
  def record(time: Instant, value: T): Unit =
    values = values.appended((time, value))
}
