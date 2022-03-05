package it.unibo.casestudy.launch
import upickle.default._
case class SimulationDescriptions(
    training: Int = 500,
    greedy: Int = 5,
    gamma: Seq[Double] = Seq(0.99),
    alphaBeta: Seq[(Double, Double)] = Seq((0.1, 0.1)),
    epsilon: Seq[Double] = Seq(0.2),
    window: Seq[Int] = Seq(5)
) {
  def total: Int = gamma.size * alphaBeta.size * epsilon.size * window.size
}

object SimulationDescriptions {
  implicit def serialize: ReadWriter[SimulationDescriptions] = macroRW[SimulationDescriptions]
}
