import scala.scalajs.js.timers._
import scalatags.JsDom.all._
import org.scalajs.dom
  
import scala.scalajs.js.annotation._
import rx._
import rx.async._
import rx.async.Platform._
import scala.concurrent.duration._

object Fiddle {
  import org.scalajs.dom

  val canvas = dom.document.getElementById("fiddle-canvas").asInstanceOf[dom.html.Canvas]
  val draw = canvas.getContext("2d")
}

case class Machine[St, Sig](
  init: St,
  onSignal: PartialFunction[(St, Sig), St],
  onRender: PartialFunction[St, Unit])(implicit val ctx: Ctx.Owner) {

  val state: Var[St] = Var(init)
  val obs = state.debounce((1000/30).millis).trigger {
    onRender(state.now)
  }
  
  def send(signal: Sig): Unit = {
    state() = onSignal(state.now, signal)
  }
}

@JSExportTopLevel("mandala")
object Mandala {

  val NUM_SIDES = 7

  case class Settings(width: Int, height: Int, color: String)

  trait State { val settings: Settings }
  case object Empty extends State { val settings = Settings(0, 0, "black") }
  case class Running(settings: Settings, inkLines: Seq[Line], colorLines: Seq[Line]) extends State
  case class Drawing(settings: Settings, line: Line, inkLines: Seq[Line], colorLines: Seq[Line]) extends State

  trait Signal
  case class Initialize(settings: Settings) extends Signal
  case class MouseMove(x: Int, y: Int) extends Signal
  case class MouseDown(x: Int, y: Int) extends Signal
  case class MouseUp(x: Int, y: Int) extends Signal
  case class ColorChange(color: String) extends Signal
  case class Resize(width: Int, height: Int) extends Signal

  case class Pt(x: Int, y: Int)
  case class Line(color: String, start: Pt, segments: Seq[Pt]) {
    def draw: Unit = {
      Fiddle.draw.beginPath()
      Fiddle.draw.lineCap = "round"
      Fiddle.draw.lineJoin = "round"
      Fiddle.draw.strokeStyle = color
      Fiddle.draw.lineWidth = if (color == "black") 5 else 15

      Fiddle.draw.moveTo(start.x, start.y)
      segments.foreach(pt => Fiddle.draw.lineTo(pt.x, pt.y))
      Fiddle.draw.stroke()
    }
  }

  def signaled: PartialFunction[(State, Signal), State] = {
    case (Empty, Initialize(settings)) =>
      Running(settings, Seq.empty, Seq.empty)

    case (state: Running, Resize(width, height)) =>
      state.copy(settings = state.settings.copy(width = width, height = height))

    case (Running(Settings(width, height, _), inks, lines), ColorChange(color)) =>
      Running(Settings(width, height, color), inks, lines)

    case (Running(settings, inks, lines), MouseDown(x, y)) =>
      Drawing(
        settings,
        Line(settings.color, Pt(x-settings.width/2, y-settings.height/2), Seq.empty),
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
      
    case (Drawing(settings, line, inks, lines), e: MouseUp) if settings.color == "black" =>
      Running(settings, inks :+ line, lines)

    case (Drawing(settings, line, inks, lines), e: MouseUp) =>
      Running(settings, inks, lines :+ line)
      
    case (state, _) => state
  }
  
  def clear: PartialFunction[State,State] = {
    case state =>
      Fiddle.draw.beginPath()
      Fiddle.draw.clearRect(0, 0, state.settings.width, state.settings.height)
      Fiddle.draw.fill()
      state
  }
  
  def setup: PartialFunction[State, State] = {
    case state =>
      Fiddle.draw.save()
      Fiddle.draw.translate(state.settings.width / 2, state.settings.height / 2)
      state
  }
  
  def teardown: PartialFunction[Unit,Unit] = {
    case s =>
      Fiddle.draw.restore()
      s
  }

  val ANGLE_DELTA = 2 * Math.PI / NUM_SIDES
  val COUNT_LIST = Range(0, NUM_SIDES)
  
  def drawLines(lines: Seq[Line]): Unit = {
    val angle = 2 * Math.PI / NUM_SIDES

    COUNT_LIST.foreach(_ => {
      Fiddle.draw.rotate(ANGLE_DELTA)
      lines.foreach(_.draw)
    })
  }
  
  def render: PartialFunction[State, Unit] = clear andThen setup andThen {
    case Running(_, inks, lines) =>
      drawLines(lines)
      drawLines(inks)

    case Drawing(settings, line, inks, lines) if settings.color == "black" =>
      drawLines(lines)
      drawLines(inks)
      drawLines(Seq(line))

    case Drawing(settings, line, inks, lines) =>
      drawLines(lines)
      drawLines(Seq(line))
      drawLines(inks)

    case _ =>
  } andThen teardown

  val machine = new Machine[State, Signal](Empty, signaled, render)

  def callback[A](fn: A => Unit) = { (evt: dom.Event) =>
    fn(evt.asInstanceOf[A])
  }

  def updateCanvasInfo(canvas: dom.html.Canvas): Unit = {
    val width = canvas.clientWidth
    val height = canvas.clientHeight
    canvas.width = width
    canvas.height = height
  }

  @JSExport
  def main(args: Array[String]): Unit = {

    Fiddle.canvas.addEventListener("mousemove", callback[dom.MouseEvent] { evt =>
      val cc = Fiddle.canvas.getBoundingClientRect()
      machine.send(MouseMove(evt.clientX.toInt - cc.left.toInt, evt.clientY.toInt - cc.top.toInt))
    })
    
    Fiddle.canvas.addEventListener("mousedown", callback[dom.MouseEvent] { evt =>
      val cc = Fiddle.canvas.getBoundingClientRect()
      machine.send(MouseDown(evt.clientX.toInt - cc.left.toInt, evt.clientY.toInt - cc.top.toInt))
    })
    
    Fiddle.canvas.addEventListener("mouseup", callback[dom.MouseEvent] { evt =>
      val cc = Fiddle.canvas.getBoundingClientRect()
      machine.send(MouseUp(evt.clientX.toInt - cc.left.toInt, evt.clientY.toInt - cc.top.toInt))
    })

    dom.window.addEventListener("resize", callback[dom.Event] { evt =>
      updateCanvasInfo(Fiddle.canvas)
      machine.send(Resize(Fiddle.canvas.width.toInt, Fiddle.canvas.height.toInt))
    })

    updateCanvasInfo(Fiddle.canvas)

    machine.send(Initialize(Settings(Fiddle.canvas.width, Fiddle.canvas.height, "black")))

    dom.document.addEventListener("touchstart", callback[dom.TouchEvent] { evt =>
      val cc = Fiddle.canvas.getBoundingClientRect()
      val t = evt.touches(0)
      machine.send(MouseDown(t.clientX.toInt - cc.left.toInt, t.clientY.toInt - cc.top.toInt))
    })

    dom.document.addEventListener("touchmove", callback[dom.TouchEvent] { evt =>
      val cc = Fiddle.canvas.getBoundingClientRect()
      val t = evt.touches(0)
      machine.send(MouseMove(t.clientX.toInt - cc.left.toInt, t.clientY.toInt - cc.top.toInt))
    })

    dom.document.addEventListener("touchend", callback[dom.TouchEvent] { evt =>
      val cc = Fiddle.canvas.getBoundingClientRect()
      machine.send(MouseUp(0, 0))
    })

    val colorSelect = dom.document.getElementById("color").asInstanceOf[dom.html.Select]
    colorSelect.addEventListener("change", callback[dom.Event] { evt =>
      machine.send(ColorChange(colorSelect.value))
    })
  }
}
