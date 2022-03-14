package it.unibo.casestudy.program
import it.unibo.casestudy.DesIncarnation._

/** Program used to a distributed leader election */
class SProgram extends AggregateProgram with StandardSensors with BlockS {
  override def main(): Any = classicGradient(S(100, nbrRange), nbrRange)
}
