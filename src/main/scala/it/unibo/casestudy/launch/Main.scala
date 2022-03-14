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

/** This is the entry used to launch the simulation. It could be configured with a json file composed of:
  *   - [SINGLE]"greedy": the last-k episode in which is deployed a greedy policy, values => N (natural)
  *   - [SINGLE]"training": the first-k episode in which the system learn the policy, values => N (natural)
  *   - [ARRAY]"gamma": gamma value of the Q-Learning algorithm, values > 0 < 1
  *   - [ARRAY]"alphaBeta": a tuple of alpha beta values. Prefers solution in which alpha = beta. Values: > 0 < 1
  *   - [ARRAY]"epsilon": devise the initial epsilon value. Values > 0 < 1
  *   - [ARRAY]"window": how many time interval the agent should consider, values => N (natural)
  *   - [ARRAY]"stableWeight": the importance of consumption against the importance of the convergence. Values: > 0 < 1
  *   - [SINGLE]"program": the aggregate program used to run simulation, values: "gradient", "cblock", "sblock"
  *   - [SINGLE]"simulation": the scenario used to launch simulations, values: "plain", "multiswap"
  *
  * Use ./config.json as reference. Each rl configuration, produce data in csv format in the folder: res/simulatioName.
  * The simulation name depends on the values passed in the simulation and it is the format of:
  * gamma-alpha-beta-epsilon-window-stableWeight.
  *
  * For each episode, is produce a csv containing:
  *   - 1 the sum of the ticks at performance in the system
  *   - 2 the total output of the system
  *   - 3 the error at each seconds w.r.t the reference (i.e. periodic update)
  *
  * 1 and 2 are exported also for the periodic round. Ad hoc instead, produces 1 2 and 3. as Rl solution Furthermore,
  * for each configuration, is exported the total error and the total ticks progression.
  */
object Main extends App {
  // Load the configuration that will be launched by the simulation, if no configuration is passed it will launched the default one
  val configurations = if (args.length != 1) {
    SimulationDescriptions()
  } else {
    val file = os.pwd / args(0)
    read[SimulationDescriptions](os.read.lines(file).mkString.stripMargin)
  }
  LoggerUtil.disableIf(configurations.total > 1)
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
  /// Simulations
  // standard simulation a.k.a. periodic
  val (standardTicks, standardOutput) =
    buildSimulation(
      fireLogic = id => RoundAtEach(id, program, zero, delta)
    ).perform()
  // adhoc simulation
  val (adHocTicks, adHocOutput) =
    buildSimulation(
      fireLogic = id => AdjustableEvaluation(id, program, zero, delta, max, delta)
    ).perform()

  scribe.warn(bgBrightRed(s"Multiple simulations.. total = ${configurations.total}"))
  // Loop for other rl based simulation
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
  // Consume an entire rl based simulation, producing the data required for plot information
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
    var recordError = Seq.empty[Double] // error progression as episodes increases
    var recordTotalTicks = Seq.empty[Double] // ticks progression as episode increases
    buildSimulation(
      fireLogic = rlRoundFunction
    ).repeat(trainingEpisodes + greedy) { (data, ep) =>
      val rlGradient = data._2
      val rlTicks = data._1
      // evaluates the error performed by each node in the system
      val error = pointWiseError(standardOutput, rlGradient).values.map(_._2).sum
      val totalTicks = totalTicksPerTrack(rlTicks).values
      // the overall tick per second consumed by the agent
      val averageTicksPerSeconds = totalTicks
        .dropRight(1)
        .zip(totalTicks.tail)
        .map { case (first, second) =>
          (second._2 - first._2)
        }
        .sum / totalTicks.size

      scribe.info(
        out(
          blue(s"episode: "),
          bold(s"$ep \n"),
          green(s"average ticks per seconds: "),
          bold(s"$averageTicksPerSeconds\n"),
          red(s"episode error: "),
          bold(s"$error\n")
        )
      )
      recordError = recordError :+ error
      recordTotalTicks = recordTotalTicks :+ averageTicksPerSeconds
      storeInCsv(
        resultFolder / configurationName / s"$rlName-$ep.csv",
        totalTicksPerTrack(rlTicks), // total ticks at each second
        totalOutputForEachStep(rlGradient), // total error produced at each second
        pointWiseError(standardOutput, rlGradient) // error percentage of the overall system
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
  // Store adhoc performance
  storeInCsv(
    resultFolder / s"$adhocName.csv",
    totalTicksPerTrack(adHocTicks),
    totalOutputForEachStep(adHocOutput),
    pointWiseError(standardOutput, adHocOutput)
  )
  // Utility
  // Store a data sequence as a csv file in which each data is placed in a row
  def storeSequence(where: os.Path, data: Seq[Double], name: String): Unit = {
    val writer = CSVWriter.open(where.toIO)
    val toStore = name +: data
    writer.writeAll(toStore.map(Seq(_)))
    writer.close()
  }
  /*
   evaluated the error percentage from a reference to another track.
   */
  def pointWiseError(
      reference: ExperimentTrace[Map[ID, Double]],
      other: ExperimentTrace[Map[ID, Double]]
  ): ExperimentTrace[Double] = {
    def zeroIfInfinity(double: Double): Double =
      if (double.isInfinity) { 0.0 }
      else { double }
    val errorPerTime = reference.values
      .zip(other.values)
      .map { case ((i, reference), (_, other)) =>
        val onlyFinite = reference
          .filter { case (_, v) => v.isFinite }
        val sumSquaredError =
          onlyFinite
            .map { case (id, _) => id -> math.pow(reference(id) - zeroIfInfinity(other(id)), 2) }
            .values
            .sum
        i -> Math.sqrt(sumSquaredError / onlyFinite.size)
      }
    val errorPerTimeTrace = new ExperimentTrace[Double](other.name + "point-wise")
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

  Analysis.main(Array("sample"))
}
