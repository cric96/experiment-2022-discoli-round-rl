package it.unibo.casestudy.launch
import com.github.tototoshi.csv.CSVWriter
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.{GradientProgram, GradientSimulation}
import it.unibo.casestudy.GradientSimulation.SimulationConfiguration
import it.unibo.casestudy.event.RLRoundEvaluation.Configuration
import it.unibo.casestudy.event.{AdjustableEvaluation, RLRoundEvaluation, RoundAtEach}
import it.unibo.casestudy.utils.ExperimentTrace.storeInCsv
import it.unibo.casestudy.utils.{ExperimentTrace, Memoize, Variable}
import scribe.output._
import LaunchConstant._
import java.time.Instant
import scala.concurrent.duration._
import scala.language.postfixOps
object Main extends App {
  val resultFolder = os.pwd / resFolder
  if (os.exists(resultFolder)) { os.remove.all(resultFolder) }
  os.makeDir.all(resultFolder)
  // Constants
  val initialEps = 0.2
  val trainingEpisodes = 500
  val gamma = 0.99
  val alpha = 0.1
  val beta = 0.1
  val temporalWindow = 5
  val greedy = 1
  def epsilon() = Variable.linearDecay(initialEps, initialEps / trainingEpisodes)
  def learn() = Variable.changeAfter(trainingEpisodes, true, false)
  val delta = 100 milliseconds
  val zero = Instant.ofEpochMilli(0)
  val max = 2 seconds

  val (standardTicks, standardOutput) =
    new GradientSimulation(
      fireLogic = id => RoundAtEach(id, new GradientProgram, zero, delta),
      config = SimulationConfiguration()
    ).perform()

  val (adHocTicks, adHocOutput) =
    new GradientSimulation(
      fireLogic = id => AdjustableEvaluation(id, new GradientProgram, zero, delta, max, delta),
      config = SimulationConfiguration()
    ).perform()

  val rlRoundFunction = Memoize[ID, RLRoundEvaluation] { id =>
    new RLRoundEvaluation(
      id,
      new GradientProgram,
      Instant.ofEpochMilli(0),
      rlConfig = Configuration(gamma, alpha, beta, epsilon(), learn()),
      temporalWindow = temporalWindow
    )
  }.andThen(_.updateVariables().reset()) // used to clear the variables at the begging of each learning process

  var recordError = Seq.empty[Double]
  var recordTotalTicks = Seq.empty[Double]
  new GradientSimulation(
    fireLogic = rlRoundFunction,
    config = SimulationConfiguration(seeds = Variable.evolveWith(Seeds(0, 0, 0), i => Seeds(i, i, i)))
  ).repeat(trainingEpisodes + greedy) { (data, ep) =>
    val rlGradient = data._2
    val rlTicks = data._1
    val error = totalError(standardOutput, rlGradient)
    val totalTicks = rlTicks.values.map(_._2).sum / rlTicks.values.size.toDouble
    scribe.info(
      out(
        blue(s"episode: "),
        bold(s"$ep \n"),
        green(s"total ticks: "),
        bold(s"$totalTicks\n"),
        red(s"episode error: "),
        bold(s"$error\n")
      )
    )
    recordError = recordError :+ error
    recordTotalTicks = recordTotalTicks :+ totalTicks
    storeInCsv(resultFolder / s"$rlName-$ep.csv", rlTicks, rlGradient)
  }.last // consume the stream
  storeSequence(resultFolder / s"$errorName.csv", recordError, errorName)
  storeSequence(resultFolder / s"$totalTicksName.csv", recordTotalTicks, totalTicksName)
  storeInCsv(resultFolder / s"$fixedName.csv", standardTicks, standardOutput)
  storeInCsv(resultFolder / s"$adhocName.csv", adHocTicks, adHocOutput)

  def storeSequence(where: os.Path, data: Seq[Double], name: String): Unit = {
    val writer = CSVWriter.open(where.toIO)
    val toStore = name +: data
    writer.writeAll(toStore.map(Seq(_)))
    writer.close()
  }

  def totalError(reference: ExperimentTrace[Double], rl: ExperimentTrace[Double]): Double = Math.sqrt(
    reference.values
      .map(_._2)
      .zip(rl.values.map(_._2))
      .map { case (correct, rl) =>
        Math.pow(correct - rl, 2)
      }
      .sum / standardOutput.values.size
  )

  Plot.main(Array())
}
