package it.unibo.casestudy.launch

import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.{MultipleSwapSimulation, Simulation, SwapSimulation}
import it.unibo.casestudy.Simulation.TicksAndOutput
import it.unibo.casestudy.program.{CProgram, GradientProgram, SProgram}

/** Factories used to spawn scenario and program */
object SimulationFactory {
  /** create a factory of simulation from a scenario
    * @param kind
    *   "plain" | "multiswap"
    * @return
    *   the factory to create simulation from id and round event
    */
  def simulationFromString(kind: String): (ID => RoundEvent) => Simulation[TicksAndOutput] = kind match {
    case "plain" => f => new SwapSimulation(f, config = SwapSimulation.SimulationConfiguration())
    case "multiswap" => f => new MultipleSwapSimulation(f, config = MultipleSwapSimulation.SimulationConfiguration())
  }

  /** create a program from a string
    * @param kind
    *   gradient | cblock | sblock
    * @return
    *   the aggregate program created
    */
  def programFromString(kind: String): AggregateProgram = kind match {
    case "gradient" => new GradientProgram
    case "cblock" => new CProgram
    case "sblock" => new SProgram
  }
}
