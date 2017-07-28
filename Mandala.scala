import scalatags.JsDom.all._
import org.scalajs.dom
import scala.scalajs.js.annotation._

// Defines the states, signals, and logic behind the mandala machine

object Mandala {

  case class Settings(width: Int, height: Int, color: String, sides: Int)

  trait State { val settings: Settings }
  case class Paused(settings: Settings, inkLines: Seq[Line], colorLines: Seq[Line]) extends State
  case class Drawing(settings: Settings, line: Line, inkLines: Seq[Line], colorLines: Seq[Line]) extends State

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

  def signaled: PartialFunction[(State, Signal), State] = {
    case (state, Initialize(settings)) =>
      Paused(settings, Seq.empty, Seq.empty)

    case (Paused(settings, _, _), Clear) =>
      Paused(settings, Seq.empty, Seq.empty)

    case (state: Paused, Resize(width, height)) =>
      state.copy(settings = state.settings.copy(width = width, height = height))

    case (Paused(Settings(width, height, _, sides), inks, lines), ColorChange(color)) =>
      Paused(Settings(width, height, color, sides), inks, lines)

    case (Paused(Settings(width, height, color, _), inks, lines), ShapeChange(sides)) =>
      Paused(Settings(width, height, color, sides), inks, lines)

    case (Paused(settings, inks, lines), MouseDown(touch, x, y)) =>
      Drawing(
        settings,
        Line(
          settings.sides,
          (if (touch) 3 else 1) * (if (settings.color == "black") 5 else 15),
          settings.color,
          Seq(Pt(x-settings.width/2, y-settings.height/2))),
        inks,
        lines)

    case (state: Drawing, Resize(width, height)) =>
      state.copy(settings = state.settings.copy(width = width, height = height))

    case (Drawing(settings, line, inks, lines), MouseMove(x, y)) =>
      Drawing(
        settings,
        line.copy(segments = line.segments :+ Pt(x - settings.width/2, y - settings.height/2)),
        inks,
        lines)
      
    case (Drawing(settings, line, inks, lines), MouseUp) if settings.color == "black" =>
      Paused(settings, inks :+ line, lines)

    case (Drawing(settings, line, inks, lines), MouseUp) =>
      Paused(settings, inks, lines :+ line)
      
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

  def render(draw: dom.CanvasRenderingContext2D, state: State): Unit = {
    draw.beginPath()
    draw.clearRect(0, 0, state.settings.width, state.settings.height)
    draw.fill()

    draw.save()
    draw.translate(state.settings.width / 2, state.settings.height / 2)

    draw.beginPath()
    draw.fillStyle = "lightgray"
    draw.arc(0, 0, 3, 0, 2 * Math.PI, false)
    draw.fill()

    state match {
      case Paused(settings, inks, lines) =>
        lines.foreach(drawLine(draw, _))
        inks.foreach(drawLine(draw, _))

      case Drawing(settings, line, inks, lines) if settings.color == "black" =>
        lines.foreach(drawLine(draw, _))
        inks.foreach(drawLine(draw, _))
        drawLine(draw, line)

      case Drawing(settings, line, inks, lines) =>
        lines.foreach(drawLine(draw, _))
        drawLine(draw, line)
        inks.foreach(drawLine(draw, _))

      case _ =>
    }

    draw.restore()

  }
}
