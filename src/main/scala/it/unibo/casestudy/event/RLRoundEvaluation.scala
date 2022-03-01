package it.unibo.casestudy.event
import it.unibo.casestudy.{DesIncarnation, ExperimentConstant}
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.event.RLRoundEvaluation.{Normal, QRLFamily, _}
import it.unibo.rl.model.{QRL, QRLImpl}

import java.time.Instant
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import scala.util.Random
class RLRoundEvaluation(val node: ID, val program: EXECUTION, val when: Instant) extends RoundEvent {
  self =>

  implicit class DoubleWithAlmostEquals(val d: Double) {
    def default = 0.00001
    def ~=(d2: Double) = (d - d2).abs < default
  }

  final private val maxWindow = 5
  protected var q: QRLFamily.QFunction = QRLFamily.QFunction(Set(EnergySaving, FullSpeed, Normal))
  protected var oldValue: Double = Double.PositiveInfinity
  protected var state: State = State(FullSpeed, Seq.empty[OutputDirection])
  protected var reinforcementLearningProcess: QRLFamily.RealtimeQLearning =
    QRLFamily.RealtimeQLearning(0.9, q, QRL.StaticParameters(0.05, 0.1))

  override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
    val currentHistory = state.history
    val context = network.context(node)
    implicit val random = context.sense[Random]("LSNS_RANDOM").get
    val deltaTime = context.sense[FiniteDuration]("LSNS_DELTA_TIME").get
    reinforcementLearningProcess.setState(state)
    network.progress(node, program)
    val currentValue = network.`export`(node).map(_.root[Double]()).getOrElse(Double.PositiveInfinity)
    val direction = if (currentValue ~= oldValue) {
      Same
    } else if (currentValue > oldValue) {
      RisingUp
    } else {
      RisingDown
    }
    val action = reinforcementLearningProcess.takeEpsGreedyAction(q)
    val nextState = State(action, (direction +: currentHistory).take(maxWindow))
    val reward = if (currentHistory.headOption.getOrElse(Same) != Same) {
      -1 * (deltaTime / EnergySaving.next)
    } else {
      -(1 - (deltaTime / EnergySaving.next))
    }
    reinforcementLearningProcess.observeEnvAndUpdateQ(q, nextState, reward)
    val nextEvent = new RLRoundEvaluation(node, program, when.plusMillis(action.next.toMillis)) {
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

  def fromSign(sign: Int): OutputDirection = if (sign == 0) {
    Same
  } else if (sign > 0) {
    RisingUp
  } else {
    RisingDown
  }
  case class State(currentSetting: WeakUpAction, history: Seq[OutputDirection])

  val QRLFamily: QRLImpl[State, WeakUpAction] = new QRLImpl[State, WeakUpAction] {}
  val globalQ = QRLFamily.QFunction(Set(EnergySaving, FullSpeed, Normal))
}
