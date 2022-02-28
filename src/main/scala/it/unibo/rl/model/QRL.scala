package it.unibo.rl.model

import Stochastics._

import scala.util.Random

trait QRL[S, A] {
  type R = Double // Reward
  type P = Double // Probability
  type Policy = S => A
  type VFunction = S => R
  /** Q is an updatable, table-oriented state-action value function, to optimise selection over certain actions */
  trait Q extends ((S, A) => R) {
    def actions: Set[A]

    def update(s: S, a: A, v: R): Q

    def greedyPolicy: Policy = s => actions.maxBy(this(s, _))

    def epsilonGreedyPolicy(epsilon: P)(implicit rand: Random): Policy = explorationPolicy(epsilon)

    def explorationPolicy(f: P)(implicit rand: Random): Policy = {
      case s if Stochastics.drawFiltered(_ < f) => Stochastics.uniformDraw(actions)
      case s => greedyPolicy(s)
    }

    def optimalVFunction: VFunction = s => actions.map(this(s, _)).max
  }
}

object QRL {
  case class QLParameter(value: Double, t: Double) {
    def epsilon = 0.01 // Math.min(0.9, 0.00 + (1- 0.00) * math.exp(-0.4 * (t/ 20.0)))
    def alpha = 0.9 // Math.max(0.05, Math.min(0.5, 0.1 - Math.log10((t+1)/100.0))) //
  }
}
