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
  val configurations = if (args.length != 1) {
    SimulationDescriptions()
  } else {
    val file = os.pwd / args(0)
    read[SimulationDescriptions](os.read.lines(file).mkString.stripMargin)
  }
  def buildSimulation(fireLogic: ID => RoundEvent): Simulation[TicksAndOutput] =
    SimulationFactory.simulationFromString(configurations.simulation)(fireLogic)
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
    weight <- configurations.stableWeight
  } {
    val configurationName =
      s"$gamma-$alpha-$beta-$initialEps-$window-$weight"
    scribe.warn(out(bgBrightCyan(s"Simulation with: " + configurationName)))
    def epsilon() = Variable.linearDecay(initialEps, initialEps / configurations.training)
    def learn() = Variable.changeAfter(configurations.training, true, false)
    def configuration = Configuration(gamma, alpha, beta, epsilon(), learn())
    val clock = System.currentTimeMillis()
    runCompleteRlSimulation(
      configuration,
      window,
      configurations.training,
      configurations.greedy,
      weight,
      configurationName
    )
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
      stableWeight: Double,
      configurationName: String
  ): Unit = {
    os.makeDir(resultFolder / configurationName)
    val rlRoundFunction = Memoize[ID, RLRoundEvaluation] { id =>
      new RLRoundEvaluation(id, program, zero, temporalWindow, stableWeight, config)
    }.andThen(_.updateVariables().reset()) // used to clear the variables at the begging of each learning process
    var recordError = Seq.empty[Double]
    var recordTotalTicks = Seq.empty[Double]
    buildSimulation(
      fireLogic = rlRoundFunction
    ).repeat(trainingEpisodes + greedy) { (data, ep) =>
      val rlGradient = data._2
      val rlTicks = data._1
      val error = pointWiseError(standardOutput, rlGradient).values.map(_._2).sum
      val totalTicks = totalTicksPerTrack(rlTicks).values.map(_._2).sum / rlTicks.values.size.toDouble
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
      storeInCsv(
        resultFolder / configurationName / s"$rlName-$ep.csv",
        totalTicksPerTrack(rlTicks),
        totalOutputForEachStep(rlGradient)
      )
    }.last // consume the stream
    storeSequence(resultFolder / configurationName / s"$errorName.csv", recordError, errorName)
    storeSequence(resultFolder / configurationName / s"$totalTicksName.csv", recordTotalTicks, totalTicksName)
  }
  // Store standard performance
  storeInCsv(
    resultFolder / s"$fixedName.csv",
    totalTicksPerTrack(standardTicks),
    totalOutputForEachStep(standardOutput)
  )
  storeInCsv(resultFolder / s"$adhocName.csv", totalTicksPerTrack(adHocTicks), totalOutputForEachStep(adHocOutput))

  def storeSequence(where: os.Path, data: Seq[Double], name: String): Unit = {
    val writer = CSVWriter.open(where.toIO)
    val toStore = name +: data
    writer.writeAll(toStore.map(Seq(_)))
    writer.close()
  }

  def pointWiseError(
      reference: ExperimentTrace[Map[ID, Double]],
      other: ExperimentTrace[Map[ID, Double]]
  ): ExperimentTrace[Double] = {
    val errorPerTime = reference.values
      .zip(other.values)
      .map { case ((i, reference), (_, rl)) =>
        val onlyFinite = reference.filter { case (id, v) => v.isFinite && rl(id).isFinite }
        val sumSquaredError =
          Math.sqrt(onlyFinite.map { case (id, _) => id -> math.pow(reference(id) - rl(id), 2) }.values.sum)
        i -> sumSquaredError / onlyFinite.size
      }
    val errorPerTimeTrace = new ExperimentTrace[Double](other.name)
    errorPerTimeTrace.values = errorPerTime
    errorPerTimeTrace
  }

  def totalOutputForEachStep(trace: ExperimentTrace[Map[ID, Double]]): ExperimentTrace[Double] = trace.convert {
    case (_, data) =>
      data.values.filter(_.isFinite).sum
  }

  def totalTicksPerTrack(ticks: ExperimentTrace[Map[ID, Int]]): ExperimentTrace[Int] = ticks.convert { case (i, d) =>
    d.values.sum
  }

  Plot.main(Array("sample"))
}
