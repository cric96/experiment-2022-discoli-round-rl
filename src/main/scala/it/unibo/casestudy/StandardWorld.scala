package it.unibo.casestudy

import DesIncarnation._
import it.unibo.scafi.config.GridSettings
object StandardWorld {
  def withRange(rows: Int, columns: Int, range: Double, sources: Set[ID] = Set.empty): SpaceAwareSimulator = {
    val simulator = simulatorFactory.gridLike(
      GridSettings(nrows = rows, ncols = columns, stepx = range, stepy = range), range)
    .asInstanceOf[SpaceAwareSimulator]
    simulator.addSensor(ExperimentConstant.RoundCount, 0)
    simulator.addSensor(ExperimentConstant.Source, false)
    simulator.chgSensorValue(ExperimentConstant.Source, sources, true)
    simulator
  }
}
