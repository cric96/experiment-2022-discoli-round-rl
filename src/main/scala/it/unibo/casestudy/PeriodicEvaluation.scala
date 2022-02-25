package it.unibo.casestudy
import scala.concurrent.duration._
import DesIncarnation._

import scala.language.postfixOps
object PeriodicEvaluation extends App {
  val count = 10
  val range = 10
  val delta = 100 milliseconds
  val totalTime = 80 seconds
  val leftmost = 1
  val world = StandardWorld.withRange(count, count, range, Set(leftmost))
  val des = new DesSimulator(world)
  val fireEvents = world.ids.map(RoundAtEach(_, new GradientProgram, des.now, delta))
  fireEvents.foreach(des.schedule)
  des.stopWhen(des.now.plusMillis(totalTime.toMillis))
  des.schedule(Exports.ExportCountEvent(des.now, 1 seconds))
  DesUtils.consume(des)
  println(des.now.toEpochMilli)
  println(des.count)
}
