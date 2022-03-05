package it.unibo.casestudy.launch
import com.github.tototoshi.csv.CSVWriter
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.Simulation
import it.unibo.casestudy.Simulation.TicksAndOutput
import it.unibo.casestudy.event.RLRoundEvaluation.Configuration
import it.unibo.casestudy.event.{AdjustableEvaluation, RLRoundEvaluation, RoundAtEach}
import it.unibo.casestudy.launch.LaunchConstant._
import it.unibo.casestudy.utils.ExperimentTrace.storeInCsv
import it.unibo.casestudy.utils.{ExperimentTrace, Memoize, Variable}
import scribe.Level
import scribe.output._
import upickle.default._

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.language.postfixOps
object Main extends App {
  def buildSimulation(fireLogic: ID => RoundEvent): Simulation[TicksAndOutput] =
    SimulationFactory.simulationFromString("plain")(fireLogic)
  val configurations = if (args.length != 1) {
    SimulationDescriptions()
  } else {
    val file = os.pwd / args(0)
    read[SimulationDescriptions](os.read.lines(file).mkString.stripMargin)
  }
  val resultFolder = os.pwd / resFolder
  if (os.exists(resultFolder)) { os.remove.all(resultFolder) }
  os.makeDir.all(resultFolder)
  // Constants
  val program = SimulationFactory.programFromString(configurations.program)
  val delta = 100 milliseconds
  val zero = Instant.ofEpochMilli(0)
  val max = 2 seconds

  val (standardTicks, standardOutput) =
    buildSimulation(
      fireLogic = id => RoundAtEach(id, program, zero, delta)
    ).perform()

  val (adHocTicks, adHocOutput) =
    buildSimulation(
      fireLogic = id => AdjustableEvaluation(id, program, zero, delta, max, delta)
    ).perform()

  if (configurations.total > 1) {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(Level.Warn))
      .replace()
  }
  scribe.warn(bgBrightRed(s"Multiple simulations.. total = ${configurations.total}"))

  for {
    gamma <- configurations.gamma
    (alpha, beta) <- configurations.alphaBeta
    initialEps <- configurations.epsilon
    window <- configurations.window
  } {
    val configurationName =
      s"$gamma-$alpha-$beta-$initialEps-$window"
    scribe.warn(out(bgBrightCyan(s"Simulation with: " + configurationName)))
    def epsilon() = Variable.linearDecay(initialEps, initialEps / configurations.training)
    def learn() = Variable.changeAfter(configurations.training, true, false)
    def configuration = Configuration(gamma, alpha, beta, epsilon(), learn())
    val clock = System.currentTimeMillis()
    runCompleteRlSimulation(configuration, window, configurations.training, configurations.greedy, configurationName)
    scribe.warn(
      bgYellow(s"Total Time: ${FiniteDuration(System.currentTimeMillis() - clock, TimeUnit.MILLISECONDS).toSeconds}")
    )
    scribe.warn(out(bgBrightGreen(s"Simulation " + configurationName + "end")))
  }

  def runCompleteRlSimulation(
      config: => Configuration,
      temporalWindow: Int,
      trainingEpisodes: Int,
      greedy: Int,
      configurationName: String
  ): Unit = {
    os.makeDir(resultFolder / configurationName)
    val rlRoundFunction = Memoize[ID, RLRoundEvaluation] { id =>
      new RLRoundEvaluation(id, program, zero, temporalWindow, config)
    }.andThen(_.updateVariables().reset()) // used to clear the variables at the begging of each learning process
    var recordError = Seq.empty[Double]
    var recordTotalTicks = Seq.empty[Double]
    buildSimulation(
      fireLogic = rlRoundFunction
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
      storeInCsv(resultFolder / configurationName / s"$rlName-$ep.csv", rlTicks, rlGradient)
    }.last // consume the stream
    storeSequence(resultFolder / configurationName / s"$errorName.csv", recordError, errorName)
    storeSequence(resultFolder / configurationName / s"$totalTicksName.csv", recordTotalTicks, totalTicksName)
  }
  // Store standard performance
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
