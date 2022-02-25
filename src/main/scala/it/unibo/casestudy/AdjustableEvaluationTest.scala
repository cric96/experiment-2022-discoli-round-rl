package it.unibo.casestudy

import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.event.AdjustableEvaluation

import scala.concurrent.duration._
import scala.language.postfixOps

object AdjustableEvaluationTest extends App {
  val count = 10
  val range = 10
  val delta = 100 milliseconds
  val totalTime = 80 seconds
  val leftmost = 1
  val rightmost = 100
  val world = StandardWorld.withRange(count, count, range, Set(leftmost))
  val des = new DesSimulator(world)
  val fireEvents = world.ids.map(AdjustableEvaluation(_, new GradientProgram, des.now, delta, 2 seconds, delta))
  fireEvents.foreach(des.schedule)
  des.stopWhen(des.now.plusMillis(totalTime.toMillis))
  des.schedule(ChangeSourceAt(des.now.plusMillis((40 seconds).toMillis), leftmost, false))
  des.schedule(ChangeSourceAt(des.now.plusMillis((40 seconds).toMillis), rightmost, true))
  des.schedule(Exports.NumericValueExport[Int](des.now, 1 seconds, ExperimentConstant.RoundCount))
  DesUtils.consume(des)
  println(des.now.toEpochMilli)
  println(des.count)
  println(world)
}
