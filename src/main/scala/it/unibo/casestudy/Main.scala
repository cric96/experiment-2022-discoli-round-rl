package it.unibo.casestudy
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.GradientSimulation.SimulationConfiguration
import it.unibo.casestudy.event.RLRoundEvaluation.Configuration
import it.unibo.casestudy.event.{AdjustableEvaluation, RLRoundEvaluation, RoundAtEach}
import it.unibo.casestudy.utils.{ExperimentTrace, Memoize, Variable}
import org.nspl._
import org.nspl.awtrenderer._
import org.nspl.data.DataSource

import java.io.File
import java.time.Instant
import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.language.postfixOps

object Main extends App {
  implicit def plotConverterUtil[T: Numeric](inQueue: Queue[(Instant, T)]): DataSource = inQueue.map {
    case (time, data) =>
      (time.getEpochSecond.toDouble, Numeric[T].toDouble(data))
  }

  def store(file: File, path: os.Path): Unit =
    os.copy.over(os.Path(file.toPath), path)
  val resultFolder = os.pwd / "res"
  if (os.exists(resultFolder)) { os.remove.all(resultFolder) }
  os.makeDir.all(resultFolder)

  val initialEps = 0.2
  val trainingEpisodes = 500
  val greedy = 1
  def epsilon() = Variable.linearDecay(initialEps, initialEps / trainingEpisodes)
  def learn() = Variable.changeAfter(trainingEpisodes, true, false)
  val delta = 100 milliseconds

  val (standardFrequency, gradientStandard) =
    new GradientSimulation(
      fireLogic = id => RoundAtEach(id, new GradientProgram, Instant.ofEpochMilli(0), delta),
      config = SimulationConfiguration()
    ).perform()

  val (adjustableFrequency, gradientAdjustable) =
    new GradientSimulation(
      fireLogic = id => AdjustableEvaluation(id, new GradientProgram, Instant.ofEpochMilli(0), delta, 2 seconds, delta),
      config = SimulationConfiguration(seeds = Variable.evolveWith(Seeds(0, 0, 0), i => Seeds(i, i, i)))
    ).perform()

  val rlRoundFunction = Memoize[ID, RLRoundEvaluation] { id =>
    new RLRoundEvaluation(
      id,
      new GradientProgram,
      Instant.ofEpochMilli(0),
      rlConfig = Configuration(gamma = 0.99, alpha = 0.1, beta = 0.1, epsilon(), learn()),
      temporalWindow = 5
    )
  }.andThen(_.updateVariables().reset())

  def totalError(reference: ExperimentTrace[Double], rl: ExperimentTrace[Double]): Double = Math.sqrt(
    reference.values
      .map(_._2)
      .zip(rl.values.map(_._2))
      .map { case (correct, rl) =>
        Math.pow(correct - rl, 2)
      }
      .sum / gradientStandard.values.size
  )

  def plot(rlGradient: ExperimentTrace[Double], rlTicks: ExperimentTrace[Int], label: String = ""): Unit = {
    def tickPerSeconds(trace: ExperimentTrace[Int]): Seq[Double] = {
      val totalTicks = trace.values.map(_._2)
      totalTicks.dropRight(1).zip(totalTicks.tail).map { case (first, second) => second - first }
    }
    val outputPlot = xyplot(
      (gradientStandard.values, List(line(color = Color.red)), InLegend("Periodic")),
      (gradientAdjustable.values, List(line(color = Color.green)), InLegend("Ad Hoc")),
      (rlGradient.values, List(line(color = Color.blue)), InLegend("Rl"))
    )(
      par(xlab = "time", ylab = "total output")
    )

    val totalTicksPlot = xyplot(
      (standardFrequency.values, List(line(color = Color.red)), InLegend("Periodic")),
      (adjustableFrequency.values, List(line(color = Color.green)), InLegend("Ad Hoc")),
      (rlTicks.values, List(line(color = Color.blue)), InLegend("Adjustable"))
    )(
      par(xlab = "time", ylab = "total ticks")
    )

    val frequencyPlot = xyplot(
      (tickPerSeconds(standardFrequency), List(line(color = Color.red)), InLegend("Periodic")),
      (tickPerSeconds(adjustableFrequency), List(line(color = Color.green)), InLegend("Ad Hoc")),
      (tickPerSeconds(rlTicks), List(line(color = Color.blue)), InLegend("Adjustable"))
    )(
      par(xlab = "time", ylab = "total frequency")
    )

    store(
      renderToFile(sequence(List(outputPlot, totalTicksPlot, frequencyPlot), TableLayout(2))),
      resultFolder / s"image-$label.png"
    )
  }

  var recordError = Seq.empty[Double]
  var recordTotalTicks = Seq.empty[Double]
  val (rlTicks, rlGradient) =
    new GradientSimulation(
      fireLogic = rlRoundFunction,
      config = SimulationConfiguration()
    ).repeat(trainingEpisodes + greedy) { (data, ep) =>
      val rlGradient = data._2
      val rlTicks = data._1
      val error = totalError(gradientStandard, rlGradient)
      val totalTicks = rlTicks.values.map(_._2).sum / rlTicks.values.size.toDouble
      println(s"simulation $ep")
      println(s"total ticks $totalTicks")
      recordError = recordError :+ error
      recordTotalTicks = recordTotalTicks :+ totalTicks
      println(s"episode error $error")
      plot(rlGradient, rlTicks, ep.toString)
    }.last

  val errorPlot = xyplot(
    (recordError, List(line(color = Color.red)), InLegend("Error"))
  )(
    par(xlab = "time", ylab = "Root Mean Squared Error")
  )

  val totalTickPlot = xyplot(
    (recordTotalTicks, List(line(color = Color.apply(255, 255, 0))), InLegend("Total ticks per second"))
  )(
    par(xlab = "time", ylab = "Ticks per seconds")
  )

  store(
    renderToFile(errorPlot),
    resultFolder / s"error.png"
  )

  store(
    renderToFile(totalTickPlot),
    resultFolder / s"ticks.png"
  )
}
