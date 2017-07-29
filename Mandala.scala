import scalatags.JsDom.all._
import org.scalajs.dom
import scala.scalajs.js.annotation._

// Defines the states, signals, and logic behind the mandala machine

object Mandala {

  case class Settings(width: Int, height: Int, color: String, sides: Int)

  trait State
  case class Paused(inkLines: Seq[Line], colorLines: Seq[Line]) extends State
  case class Drawing(line: Line, inkLines: Seq[Line], colorLines: Seq[Line]) extends State

  case class World(settings: Settings, state: State)

  trait Signal
  case class Initialize(settings: Settings)            extends Signal
  case class MouseDown(touch: Boolean, x: Int, y: Int) extends Signal
  case class MouseMove(x: Int, y: Int)                 extends Signal
  case object MouseUp                                  extends Signal
  case class ColorChange(color: String)                extends Signal
  case class ShapeChange(shape: Int)                   extends Signal
  case class Resize(width: Int, height: Int)           extends Signal
  case object Clear                                    extends Signal

  case class Pt(x: Int, y: Int)
  case class Line(sides: Int, size: Int, color: String, segments: Seq[Pt])

  def signaled: PartialFunction[(World, Signal), World] = {
    case (_, Initialize(settings)) =>
      World(settings, Paused(Seq.empty, Seq.empty))

    case (World(settings, state), Clear) =>
      World(settings, Paused(Seq.empty, Seq.empty))

    case (World(Settings(_, _, color, sides), state), Resize(width, height)) =>
      World(Settings(width, height, color, sides), state)

    case (World(Settings(w, h, _, sides), state), ColorChange(color)) =>
      World(Settings(w, h, color, sides), state)

    case (World(Settings(w, h, c, _), state), ShapeChange(sides)) =>
      World(Settings(w, h, c, sides), state)

    case (World(settings, Paused(inks, lines)), MouseDown(touch, x, y)) =>
      World(
        settings,
        Drawing(
          Line(
            settings.sides,
            (if (touch) 3 else 1) * (if (settings.color == "black") 5 else 15),
            settings.color,
            Seq(Pt(x-settings.width/2, y-settings.height/2))),
          inks,
          lines))

    case (World(settings, Drawing(line, inks, lines)), MouseMove(x, y)) =>
      World(
        settings,
        Drawing(
          line.copy(segments = line.segments :+ Pt(x - settings.width/2, y - settings.height/2)),
          inks,
          lines))
      
    case (World(settings, Drawing(line, inks, lines)), MouseUp) if settings.color == "black" =>
      World(settings, Paused(inks :+ line, lines))

    case (World(settings, Drawing(line, inks, lines)), MouseUp) =>
      World(settings, Paused(inks, lines :+ line))
      
    case (state, _) => state
  }
  
  def drawLine(draw: dom.CanvasRenderingContext2D, line: Line): Unit = {
    draw.lineCap = "round"
    draw.lineJoin = "round"
    draw.strokeStyle = line.color
    draw.lineWidth = line.size

    val ANGLE_DELTA = 2 * Math.PI / line.sides

    Range(0, line.sides) foreach { _ =>
      draw.rotate(ANGLE_DELTA)

      draw.beginPath()

      draw.moveTo(line.segments.head.x, line.segments.head.y)
      line.segments.foreach(pt => draw.lineTo(pt.x, pt.y))

      draw.stroke()
    }
  }

  def render(draw: dom.CanvasRenderingContext2D, world: World): Unit = {
    val state = world.state
    val settings = world.settings

    draw.beginPath()
    draw.clearRect(0, 0, settings.width, settings.height)
    draw.fill()

    draw.save()
    draw.translate(settings.width / 2, settings.height / 2)

    draw.beginPath()
    draw.fillStyle = "lightgray"
    draw.arc(0, 0, 3, 0, 2 * Math.PI, false)
    draw.fill()

    state match {
      case Paused(inks, lines) =>
        lines.foreach(drawLine(draw, _))
        inks.foreach(drawLine(draw, _))

      case Drawing(line, inks, lines) if settings.color == "black" =>
        lines.foreach(drawLine(draw, _))
        inks.foreach(drawLine(draw, _))
        drawLine(draw, line)

      case Drawing(line, inks, lines) =>
        lines.foreach(drawLine(draw, _))
        drawLine(draw, line)
        inks.foreach(drawLine(draw, _))

      case _ =>
    }

    draw.restore()
  }
}
