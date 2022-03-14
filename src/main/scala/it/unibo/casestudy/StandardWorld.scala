package it.unibo.casestudy

import DesIncarnation._
import it.unibo.casestudy.utils.ExperimentConstant
import it.unibo.scafi.config.GridSettings

/** An utility for creating the simulated world for our synthetic scenario. */
object StandardWorld {
  /** Create a world in which each node have a neighborhood range.
    * @param rows
    *   the total number of row in which the nodes are deployed
    * @param columns
    *   the total number of columns in which the nodes are deployed
    * @param range
    *   the neighbourhood range of each node
    * @param sources
    *   the set of source in the systems
    * @param seeds
    *   the seed used for the simulation purpose
    * @return
    *   the simulated grid world world.
    */
  def withRange(
      rows: Int,
      columns: Int,
      range: Double,
      sources: Set[ID] = Set.empty,
      seeds: Seeds = Seeds()
  ): SpaceAwareSimulator = {
    val simulator = simulatorFactory
      .gridLike(GridSettings(nrows = rows, ncols = columns, stepx = range, stepy = range), range, seeds = seeds)
      .asInstanceOf[SpaceAwareSimulator]

    simulator.addSensor(ExperimentConstant.RoundCount, 0)
    simulator.addSensor(ExperimentConstant.Source, false)
    simulator.chgSensorValue(ExperimentConstant.Source, sources, true)
    simulator
  }
}
