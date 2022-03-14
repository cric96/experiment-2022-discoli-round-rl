package it.unibo.casestudy

import it.unibo.casestudy.DesIncarnation.ID
import it.unibo.casestudy.utils.ExperimentTrace

/** A generic interface of simulation
  * @tparam E
  *   the data exported from one simulated episode
  */
trait Simulation[E] {
  def perform(): E
  final def repeat(times: Int)(progressEvaluation: (E, Int) => Unit): Seq[E] =
    LazyList
      .continually(perform())
      .zipWithIndex
      .tapEach { case (index, elem) => progressEvaluation(index, elem) }
      .tapEach(_ => updateAfter())
      .map(_._1)
      .take(times)
  def updateAfter(): Unit
}

object Simulation {
  case class WorldSetting(size: Int, range: Double)
  // standard export
  type TicksAndOutput = (ExperimentTrace[Map[ID, Int]], ExperimentTrace[Map[ID, Double]])
}
