package it.unibo.casestudy.event
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.event.RLRoundEvaluation.{Normal, _}
import it.unibo.casestudy.utils.RichDouble._
import it.unibo.casestudy.utils.Variable.V
import it.unibo.casestudy.{DesIncarnation, ExperimentConstant}
import it.unibo.rl.model.{QRL, QRLImpl}

import java.time.Instant
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import scala.util.Random

class RLRoundEvaluation(
    val node: ID,
    val program: EXECUTION,
    val when: Instant,
    val temporalWindow: Int = 5,
    rlConfig: Configuration
) extends RoundEvent {
  self =>
  import rlConfig._
  protected var q: QRLFamily.QFunction = QRLFamily.QFunction(Set(EnergySaving, FullSpeed, Normal))
  protected var oldValue: Double = Double.PositiveInfinity
  protected var state: State = State(FullSpeed, Seq.empty[OutputDirection])
  protected var reinforcementLearningProcess: QRLFamily.RealtimeQLearning =
    QRLFamily.RealtimeQLearning(gamma, q, QRL.StaticParameters(epsilon, alpha))

  override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
    // ARRANGE
    val context = network.context(node)
    implicit val random: Random = context.sense[Random]("LSNS_RANDOM").get
    val currentHistory = state.history
    reinforcementLearningProcess.setState(state)
    // ROUND EVALUATION
    network.progress(node, program)
    // EVAL
    val currentValue = network.`export`(node).map(_.root[Double]()).getOrElse(Double.PositiveInfinity)
    val direction = outputTemporalDirection(currentValue)
    val action = if (learn) {
      reinforcementLearningProcess.takeEpsGreedyAction(q)
    } else {
      reinforcementLearningProcess.takeGreedyAction(q)
    }
    val nextState = State(action, (direction +: currentHistory).take(temporalWindow))
    val rewardValue = reward(context)
    // IMPROVE
    if (learn) { reinforcementLearningProcess.observeEnvAndUpdateQ(q, nextState, rewardValue) }
    // ACT
    val nextEvent =
      new RLRoundEvaluation(node, program, when.plusMillis(action.next.toMillis), temporalWindow, rlConfig) {
        this.oldValue = currentValue
        this.state = nextState
        this.q = self.q
        this.reinforcementLearningProcess = self.reinforcementLearningProcess
      }
    network.chgSensorValue(
      ExperimentConstant.RoundCount,
      Set(node),
      network.context(node).sense[Int](ExperimentConstant.RoundCount).get + 1
    )
    Some(nextEvent)
  }

  def reset(): RLRoundEvaluation = {
    oldValue = Double.PositiveInfinity
    state = State(FullSpeed, Seq.empty[OutputDirection])
    this
  }

  def updateVariables(): RLRoundEvaluation = {
    rlConfig.update()
    this
  }

  private def reward(context: Context): Double = {
    val deltaTime = context.sense[FiniteDuration]("LSNS_DELTA_TIME").get
    if (state.history.headOption.getOrElse(Same) != Same) {
      -1 * (deltaTime / EnergySaving.next)
    } else {
      -(1 - (deltaTime / EnergySaving.next))
    }
  }

  private def outputTemporalDirection(current: Double): OutputDirection = if (current ~= oldValue) {
    Same
  } else if (current > oldValue) {
    RisingUp
  } else {
    RisingDown
  }
}

object RLRoundEvaluation {
  sealed abstract class WeakUpAction(val next: FiniteDuration)
  object EnergySaving extends WeakUpAction(1 seconds)
  object FullSpeed extends WeakUpAction(100 milliseconds)
  object Normal extends WeakUpAction(200 milliseconds)

  trait OutputDirection
  object Same extends OutputDirection
  object RisingUp extends OutputDirection
  object RisingDown extends OutputDirection

  case class State(currentSetting: WeakUpAction, history: Seq[OutputDirection])

  val QRLFamily: QRLImpl[State, WeakUpAction] = new QRLImpl[State, WeakUpAction] {}

  class Configuration(
      val gamma: V[Double],
      val alpha: V[Double],
      val epsilon: V[Double],
      val learn: V[Boolean] = true
  ) {
    def update(): Unit = gamma :: alpha :: epsilon :: learn :: Nil foreach (_.next())
  }

  object Configuration {
    def apply(gamma: V[Double], alpha: V[Double], epsilon: V[Double], learn: V[Boolean] = true): Configuration =
      new Configuration(gamma, alpha, epsilon, learn)
  }
}
