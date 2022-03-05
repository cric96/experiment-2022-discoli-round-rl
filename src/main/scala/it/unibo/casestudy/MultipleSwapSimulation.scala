package it.unibo.casestudy

import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.SwapSimulation.SimulationConfiguration
import it.unibo.casestudy.Simulation.{TicksAndOutput, WorldSetting}
import it.unibo.casestudy.event.ChangeSourceAt
import it.unibo.casestudy.gui.MiniGui
import it.unibo.casestudy.utils.Variable.V
import it.unibo.casestudy.utils.{DesUtils, ExperimentConstant, ExperimentTrace}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

class MultipleSwapSimulation(fireLogic: ID => RoundEvent, config: MultipleSwapSimulation.SimulationConfiguration)
    extends Simulation[TicksAndOutput] {
  import config._
  import config.worldSetting._
  override def perform(): (ExperimentTrace[Int], ExperimentTrace[Double]) = {
    val world = StandardWorld.withRange(size, size, range, Set.empty, seeds)
    world.clearExports()
    val leftmost = world.ids.min
    val rightmost = world.ids.max
    val otherCorner = rightmost - size - 1
    val lastCorner = leftmost + size - 1
    val center = leftmost + rightmost / 2
    val des = new DesSimulator(world)
    val fireEvents = des.network.ids.map(fireLogic(_))
    val roundCount =
      Exports.NumericValueExport.fromSensor[Int](des.now, sampleFrequency, ExperimentConstant.RoundCount)
    val totalGradient = Exports.NumericValueExport.`export`[Double](des.now, sampleFrequency)
    val turnOnRLeft = ChangeSourceAt(des.now, center, value = true)
    val toTurn = leftmost :: rightmost :: otherCorner :: lastCorner :: Nil
    val turnsAfter = toTurn map {
      event.ChangeSourceAt(des.now.plusMillis(switchAt.toMillis), _, value = true)
    }
    val lastSwitch = toTurn map {
      event.ChangeSourceAt(des.now.plusMillis(lastSwitchAt.toMillis), _, value = false)
    }
    des.schedule(turnOnRLeft)
    des.schedule(roundCount)
    des.schedule(totalGradient)
    turnsAfter.foreach(des.schedule)
    fireEvents.foreach(des.schedule)
    lastSwitch.foreach(des.schedule)
    des.stopWhen(des.now.plusMillis(endWhen.toMillis).plusNanos(1)) // enable safe conclusion
    DesUtils.consume(des)
    (roundCount.trace, totalGradient.trace)
  }

  override def updateAfter(): Unit = seeds.next()
}

object MultipleSwapSimulation {
  case class SimulationConfiguration(
      worldSetting: WorldSetting = WorldSetting(10, 10),
      sampleFrequency: FiniteDuration = 1 seconds,
      seeds: V[Seeds] = Seeds(0L, 0L, 0L),
      endWhen: FiniteDuration = 90 seconds,
      switchAt: FiniteDuration = 30 seconds,
      lastSwitchAt: FiniteDuration = 60 seconds
  ) {}
}
