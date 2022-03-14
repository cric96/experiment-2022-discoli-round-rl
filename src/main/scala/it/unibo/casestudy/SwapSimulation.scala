package it.unibo.casestudy

import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.Simulation.{TicksAndOutput, WorldSetting}
import it.unibo.casestudy.SwapSimulation.SimulationConfiguration
import it.unibo.casestudy.event.ChangeSourceAt
import it.unibo.casestudy.utils.Variable.V
import it.unibo.casestudy.utils.{DesUtils, ExperimentConstant}

import scala.concurrent.duration._
import scala.language.postfixOps

/** The simulation in which the system changes the source set at the time config.switchAt. Particularly, at the time
  * config.swichAt a new source appears (the rightmost in the grid system).
  * @param fireLogic:
  *   out the system should launch local rounds
  * @param config
  *   the configuration of the simulation
  */
class SwapSimulation(fireLogic: ID => RoundEvent, config: SimulationConfiguration) extends Simulation[TicksAndOutput] {
  import config._
  import config.worldSetting._
  override def perform(): TicksAndOutput = {
    val world = StandardWorld.withRange(size, size, range, Set.empty, seeds)
    val leftmost = world.ids.min
    val rightmost = world.ids.max
    val des = new DesSimulator(world)
    val fireEvents = des.network.ids.map(fireLogic(_))
    val roundCount =
      Exports.NumericValueExport.fromSensor[Int](des.now, sampleFrequency, ExperimentConstant.RoundCount)
    val totalGradient = Exports.NumericValueExport.`export`[Double](des.now, sampleFrequency)
    val turnOnRLeft = ChangeSourceAt(des.now, leftmost, value = true)
    val turnOnRight = event.ChangeSourceAt(des.now.plusMillis(switchAt.toMillis), rightmost, value = true)
    des.schedule(turnOnRLeft)
    des.schedule(roundCount)
    des.schedule(totalGradient)
    des.schedule(turnOnRight)
    fireEvents.foreach(des.schedule)
    des.stopWhen(des.now.plusMillis(endWhen.toMillis).plusNanos(1)) // enable safe conclusion
    DesUtils.consume(des)
    (roundCount.trace, totalGradient.trace)
  }

  override def updateAfter(): Unit = seeds.next()
}

object SwapSimulation {
  case class SimulationConfiguration(
      worldSetting: WorldSetting = WorldSetting(10, 10),
      sampleFrequency: FiniteDuration = 1 seconds,
      seeds: V[Seeds] = Seeds(0L, 0L, 0L),
      endWhen: FiniteDuration = 80 seconds,
      switchAt: FiniteDuration = 40 seconds
  ) {}
}
