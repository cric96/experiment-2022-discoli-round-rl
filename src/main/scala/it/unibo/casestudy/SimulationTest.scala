package it.unibo.casestudy
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.event.{AdjustableEvaluation, RoundAtEach}
import it.unibo.casestudy.utils.ExperimentTrace

import java.time.Instant
import scala.concurrent.duration._
import scala.language.postfixOps
import org.nspl._
import org.nspl.awtrenderer._

object SimulationTest extends App {
  val count = 10
  val range = 10
  val delta = 100 milliseconds
  val totalTime = 80 seconds
  val leftmost = 1
  def newSimulator(fireLogic: ID => RoundEvent): (ExperimentTrace[Int], ExperimentTrace[Double]) = {
    val world = StandardWorld.withRange(count, count, range, Set(leftmost))
    val des = new DesSimulator(world)
    val fireEvents = des.network.ids.map(fireLogic(_))
    fireEvents.foreach(des.schedule)
    des.stopWhen(des.now.plusMillis(totalTime.toMillis))
    val roundCount = Exports.NumericValueExport.fromSensor[Int](des.now, 1 seconds, ExperimentConstant.RoundCount)
    val totalGradient = Exports.NumericValueExport.`export`[Double](des.now, 1 seconds)
    des.schedule(roundCount)
    des.schedule(totalGradient)
    DesUtils.consume(des)
    (roundCount.trace, totalGradient.trace)
  }

  val (standardFrequency, gradientStandard) =
    newSimulator(id => RoundAtEach(id, new GradientProgram, Instant.ofEpochMilli(0), delta))
  val (adjustableFrequency, adjustableGradient) =
    newSimulator(id => AdjustableEvaluation(id, new GradientProgram, Instant.ofEpochMilli(0), delta, 2 seconds, delta))

  val standardPlot = gradientStandard.values.map { case (time, data) => time.toEpochMilli.toDouble -> data.toDouble }
  val adjustablePlot = adjustableGradient.values.map { case (time, data) =>
    time.toEpochMilli.toDouble -> data.toDouble
  }
  val plot = xyplot(
    (standardPlot.toList, List(line()), InLegend("Periodic")),
    (adjustablePlot.toList, List(line()), InLegend("Adjustable"))
  )(
    par(xlab = "x axis label", ylab = "y axis label")
  )

  show(plot)
}
