package it.unibo.casestudy.launch
import upickle.default._

/** The description a sequence of simulation launched for analysing the result Rl solution
  * @param training
  *   the first-k episode in which the system learn the policy, values => N (natural)
  * @param greedy
  *   the last-k episode in which is deployed a greedy policy, values => N (natural)
  * @param simulation
  *   the scenario used to launch simulations, values: "plain", "multiswap"
  * @param program
  *   the aggregate program used to run simulation, values: "gradient", "cblock", "sblock"
  * @param gamma
  *   gamma value of the Q-Learning algorithm, values > 0 < 1
  * @param alphaBeta
  *   a tuple of alpha beta values. Prefers solution in which alpha = beta. Values: > 0 < 1
  * @param stableWeight
  *   the importance of consumption against the importance of the convergence. Values: > 0 < 1
  * @param epsilon
  *   devise the initial epsilon value. Values > 0 < 1
  * @param window
  *   how many time interval the agent should consider, values => N (natural)
  */
case class SimulationDescriptions(
    training: Int = 100,
    greedy: Int = 1,
    simulation: String = "plain",
    program: String = "gradient",
    gamma: Seq[Double] = Seq(0.99),
    alphaBeta: Seq[(Double, Double)] = Seq((0.1, 0.1)),
    stableWeight: Seq[Double] = Seq(0.99),
    epsilon: Seq[Double] = Seq(0.2),
    window: Seq[Int] = Seq(5)
) {
  def total: Int = gamma.size * alphaBeta.size * epsilon.size * window.size * stableWeight.size
}

object SimulationDescriptions {
  /** helper used to serialize the data */
  implicit def serialize: ReadWriter[SimulationDescriptions] = macroRW[SimulationDescriptions]
}
