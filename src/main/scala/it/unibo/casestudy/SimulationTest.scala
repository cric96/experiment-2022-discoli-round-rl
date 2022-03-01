package it.unibo.casestudy
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.event.RLRoundEvaluation.Configuration
import it.unibo.casestudy.event.{AdjustableEvaluation, ChangeSourceAt, RLRoundEvaluation, RoundAtEach}
import it.unibo.casestudy.utils.{DesUtils, ExperimentTrace, Memoize}

import java.time.Instant
import scala.concurrent.duration._
import scala.language.postfixOps
import org.nspl._
import org.nspl.awtrenderer._

object SimulationTest extends App {
  val count = 10
  val range = 10
  val delta = 100 milliseconds
  val sampleFrequency = 1 seconds
  val totalTime = 80 seconds
  val switchTime = totalTime / 2
  val leftmost = 1
  val rightmost = 100

  def newSimulator(fireLogic: ID => RoundEvent): (ExperimentTrace[Int], ExperimentTrace[Double]) = {
    val world = StandardWorld.withRange(count, count, range, Set(leftmost))
    val des = new DesSimulator(world)
    val fireEvents = des.network.ids.map(fireLogic(_))
    val roundCount = Exports.NumericValueExport.fromSensor[Int](des.now, sampleFrequency, ExperimentConstant.RoundCount)
    val totalGradient = Exports.NumericValueExport.`export`[Double](des.now, sampleFrequency)
    val turnOffLeft = ChangeSourceAt(des.now.plusMillis(switchTime.toMillis), leftmost, value = false)
    val turnOnRight = event.ChangeSourceAt(des.now.plusMillis(switchTime.toMillis), rightmost, value = true)
    des.schedule(roundCount)
    des.schedule(totalGradient)
    des.schedule(turnOffLeft)
    des.schedule(turnOnRight)
    fireEvents.foreach(des.schedule)
    des.stopWhen(des.now.plusMillis(totalTime.toMillis))
    DesUtils.consume(des)
    (roundCount.trace, totalGradient.trace)
  }

  val (standardFrequency, gradientStandard) =
    newSimulator(Memoize[ID, RoundEvent] { id =>
      AdjustableEvaluation(id, new GradientProgram, Instant.ofEpochMilli(0), delta, 2 seconds, delta)
    })
  val (adjustableFrequency, adjustableGradient) =
    newSimulator(Memoize[ID, RLRoundEvaluation] { id =>
      new RLRoundEvaluation(id, new GradientProgram, Instant.ofEpochMilli(0), rlConfig = Configuration(0.9, 0.1, 0.01))
    }.andThen(_.reset()))

  // AdjustableEvaluation(id, new GradientProgram, Instant.ofEpochMilli(0), delta, 2 seconds, delta)

  val fixedOutputPlot = gradientStandard.values.map { case (time, data) => time.getEpochSecond.toDouble -> data }
  val adjustableOutputPlot = adjustableGradient.values.map { case (time, data) =>
    time.getEpochSecond.toDouble -> data
  }

  val fixedFrequencyPlot = standardFrequency.values.map { case (time, data) =>
    time.getEpochSecond.toDouble -> data.toDouble
  }
  val adjustableFrequencyPlot = adjustableFrequency.values.map { case (time, data) =>
    time.getEpochSecond.toDouble -> data.toDouble
  }
  val outputPlot = xyplot(
    (fixedOutputPlot.toList, List(line(color = Color.red)), InLegend("Periodic")),
    (adjustableOutputPlot.toList, List(line(color = Color.blue)), InLegend("Adjustable"))
  )(
    par(xlab = "time", ylab = "total output")
  )

  val frequencyPlot = xyplot(
    (fixedFrequencyPlot.toList, List(line(color = Color.red)), InLegend("Periodic")),
    (adjustableFrequencyPlot.toList, List(line(color = Color.blue)), InLegend("Adjustable"))
  )(
    par(xlab = "time", ylab = "total frequency")
  )

  val path = renderToFile(sequence(List(outputPlot, frequencyPlot), TableLayout(2))).toPath
  os.copy.over(os.Path(path), os.pwd / "image.png")
}
