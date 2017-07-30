import scalatags.JsDom.all._
import org.scalajs.dom
import scala.scalajs.js.annotation._

// Defines the states, signals, and logic behind the mandala machine

object Mandala {
  case class Settings(width: Int = 0, height: Int = 0, color: Color = Ink, sides: Int = 7, tick: Int = 0)
  sealed trait Color { def css(tick: Int): String }
  case object Ink extends Color {
    def css(tick: Int) = "black"
  }
  case class Phase(offset: Int) extends Color {
    def css(tick: Int) = s"hsl(${(tick + offset * 90) % 360}, 100%, 50%)"
  }

  trait State
  case class Paused(inkLines: Seq[Line], colorLines: Seq[Line]) extends State
  case class Drawing(line: Line, inkLines: Seq[Line], colorLines: Seq[Line]) extends State

  case class World(settings: Settings, state: State)

  trait Signal
  case class Initialize(settings: Settings)            extends Signal
  case class MouseDown(touch: Boolean, x: Int, y: Int) extends Signal
  case class MouseMove(x: Int, y: Int)                 extends Signal
  case object MouseUp                                  extends Signal
  case class ColorChange(color: Color)                 extends Signal
  case class ShapeChange(shape: Int)                   extends Signal
  case class Resize(width: Int, height: Int)           extends Signal
  case object Clear                                    extends Signal
  case object Tick                                     extends Signal

  case class Pt(x: Int, y: Int)
  case class Line(sides: Int, size: Int, color: Color, segments: Seq[Pt])
}

case class Mandala(draw: dom.CanvasRenderingContext2D) {
  import Mandala._

  def signaled: PartialFunction[(Signal, World), World] = {
    case (Initialize(settings), _) =>
      World(settings, Paused(Seq.empty, Seq.empty))

    case (Clear, World(settings, state)) =>
      World(settings, Paused(Seq.empty, Seq.empty))

    case (Tick, World(settings, state)) =>
      World(settings.copy(tick = settings.tick+1), state)

    case (Resize(w, h), World(settings, state)) =>
      World(settings.copy(width = w, height = h), state)

    case (ColorChange(c), World(settings, state)) =>
      World(settings.copy(color = c), state)

    case (ShapeChange(s), World(settings, state)) =>
      World(settings.copy(sides = s), state)

    case (MouseDown(touch, x, y), World(settings, Paused(inks, lines))) =>
      World(
        settings,
        Drawing(
          Line(
            settings.sides,
            (if (touch) 3 else 1) * (if (settings.color == Ink) 5 else 15),
            settings.color,
            Seq(Pt(x-settings.width/2, y-settings.height/2))),
          inks,
          lines))

    case (MouseMove(x, y), World(settings, Drawing(line, inks, lines))) =>
      World(
        settings,
        Drawing(
          line.copy(segments = line.segments :+ Pt(x - settings.width/2, y - settings.height/2)),
          inks,
          lines))
      
    case (MouseUp, World(settings, Drawing(line, inks, lines))) if settings.color == Ink =>
      World(settings, Paused(inks :+ line, lines))

    case (MouseUp, World(settings, Drawing(line, inks, lines))) =>
      World(settings, Paused(inks, lines :+ line))

    case (_, world) => world
  }
  
  def drawLine(tick: Int, line: Line): Unit = {
    draw.lineCap = "round"
    draw.lineJoin = "round"
    draw.strokeStyle = line.color.css(tick)
    draw.lineWidth = line.size

    println(s"drawLine ${line.color} => ${line.color.css(tick)} => $tick")

    val ANGLE_DELTA = 2 * Math.PI / line.sides

    Range(0, line.sides) foreach { _ =>
      draw.rotate(ANGLE_DELTA)

      draw.beginPath()

      draw.moveTo(line.segments.head.x, line.segments.head.y)
      line.segments.foreach(pt => draw.lineTo(pt.x, pt.y))

      draw.stroke()
    }
  }

  def render(world: World): Unit = {
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
        lines.foreach(drawLine(settings.tick, _))
        inks.foreach(drawLine(settings.tick, _))

      case Drawing(line, inks, lines) if settings.color == "black" =>
        lines.foreach(drawLine(settings.tick, _))
        inks.foreach(drawLine(settings.tick, _))
        drawLine(settings.tick, line)

      case Drawing(line, inks, lines) =>
        lines.foreach(drawLine(settings.tick, _))
        drawLine(settings.tick, line)
        inks.foreach(drawLine(settings.tick, _))

      case _ =>
    }

    draw.restore()
  }
}
