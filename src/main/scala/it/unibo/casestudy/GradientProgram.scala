package it.unibo.casestudy
import DesIncarnation._

class GradientProgram extends AggregateProgram with StandardSensors with BlockG with Gradients {
  override def main(): Double = classicGradient(sense("source"), nbrRange) // to understand if we should use range or hop count
}
