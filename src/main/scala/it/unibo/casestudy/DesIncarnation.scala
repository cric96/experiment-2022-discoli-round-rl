package it.unibo.casestudy

import it.unibo.scafi.incarnations.BasicAbstractSimulationIncarnation
import it.unibo.scafi.lib.StandardLibrary
import it.unibo.scafi.simulation.{DiscreteEventSimulation, SpatialSimulation}
import it.unibo.scafi.space.{BasicSpatialAbstraction, Point3D}

object DesIncarnation
    extends BasicAbstractSimulationIncarnation
    with SpatialSimulation
    with BasicSpatialAbstraction
    with DiscreteEventSimulation
    with StandardLibrary {
  override type P = Point3D
  implicit override val idBounded: DesIncarnation.Builtins.Bounded[Int] = DesIncarnation.Builtins.Bounded.of_i
}
