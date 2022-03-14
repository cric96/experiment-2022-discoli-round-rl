package it.unibo.casestudy.program

import it.unibo.casestudy.DesIncarnation._

/** Program used to collect data */
class CProgram extends AggregateProgram with StandardSensors with BlockG with BlockC with Gradients {
  override def main(): Double = {
    val g = classicGradient(sense("source"), nbrRange) // to understand if we should use range or hop count
    C[Double, Double](g, _ + _, 1, 0)
  }
}
