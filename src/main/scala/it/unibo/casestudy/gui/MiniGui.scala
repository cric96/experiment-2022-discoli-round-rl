package it.unibo.casestudy.gui
import it.unibo.casestudy.DesIncarnation
import it.unibo.casestudy.DesIncarnation._
import it.unibo.casestudy.gui.MiniGui.Canvas

import java.awt.Color
import java.time.Instant
import scala.swing.{Dimension, Frame, Graphics2D, MainFrame, Panel, Swing}
class MiniGui(width: Int, height: Int) {
  val canvas = new Canvas()
  val frame = new MainFrame {
    contents = canvas
    size = new Dimension(width, height)
    visible = true
  }
  def render(world: SpaceAwareSimulator): Unit =
    Swing.onEDT(canvas.repaintWorld(world))
}

object MiniGui {
  class Canvas extends Panel {
    var centerColor = Color.yellow

    var world: Option[SpaceAwareSimulator] = None

    override def paintComponent(g: Graphics2D) {
      // Start by erasing this Canvas
      g.clearRect(0, 0, size.width, size.height)
      world.foreach(w => paint(g, w))
    }

    private def paint(g: Graphics2D, world: SpaceAwareSimulator): Unit = {
      val positions = world.space.elemPositions.values
      val min = positions.min
      val max = positions.max
      val ratioX = size.width / (max._1 - min._1)
      val ratioY = size.height / (max._2 - min._2)
      g.setColor(Color.black)
      positions.foreach { p =>
        val dx = p._1.toInt * ratioX.toInt
        val dy = p._2.toInt * ratioY.toInt
        g.fillOval(dx, dy, sizeOval, sizeOval)
        g.drawString(world.space.getAt(p).get.toString, dx, dy)
      }
    }
    /** Add a "dart" to list of things to display */
    def repaintWorld(world: SpaceAwareSimulator) {
      this.world = Some(world)
      repaint()
    }
  }
  val sizeOval = 4
  lazy val gui = new MiniGui(800, 600)
  def guiEvent(at: Instant): Event = new Event {
    override val when: Instant = at
    override def act(network: DesIncarnation.NetworkSimulator): Option[DesIncarnation.Event] = {
      gui.render(network.asInstanceOf[SpaceAwareSimulator])
      Some(guiEvent(at.plusSeconds(1)))
    }
  }
}
