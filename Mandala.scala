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

  val canvas = dom.document.getElementById("fiddle-canvas")
    .asInstanceOf[dom.html.Canvas]
  val draw = canvas.getContext("2d")
}

case object Machine {
  def onSignal[St, Sig](handler: PartialFunction[(St, Sig), St]) = Machine1[St,Sig]((st,sig) => handler((st,sig)))
  def onSignal[St, Sig](handler: (St, Sig) => St)                = Machine1[St,Sig](handler)

  case class Machine1[St, Sig](onSignal: (St, Sig) => St) {
    def onRender(handler: PartialFunction[St, _]) = Machine2[St,Sig](onSignal, st => handler(st))
    def onRender(handler: St => _)                = Machine2[St,Sig](onSignal, handler)
  }

  case class Machine2[St, Sig](onSignal: (St, Sig) => St, onRender: St => _) {
    def init(initialState: St)(implicit ctx: Ctx.Owner) =
      Machine(initialState, onSignal, onRender)(ctx)
  }
}

case class Machine[St, Sig](
  init: St,
  onSignal: (St, Sig) => St,
  onRender: St => _)(implicit val ctx: Ctx.Owner) {

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

  case class Settings(width: Int, height: Int, color: String, sides: Int) {
    val ANGLE_DELTA = 2 * Math.PI / sides
    val COUNT_LIST = Range(0, sides)
  }

  trait State { val settings: Settings }
  case object Empty extends State { val settings = Settings(0, 0, "black", 3) }
  case class Running(settings: Settings, inkLines: Seq[Line], colorLines: Seq[Line]) extends State
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
  case class Line(size: Int, color: String, start: Pt, segments: Seq[Pt]) {
    def draw: Unit = {
      Fiddle.draw.beginPath()
      Fiddle.draw.lineCap = "round"
      Fiddle.draw.lineJoin = "round"
      Fiddle.draw.strokeStyle = color
      Fiddle.draw.lineWidth = size

      Fiddle.draw.moveTo(start.x, start.y)
      segments.foreach(pt => Fiddle.draw.lineTo(pt.x, pt.y))
      Fiddle.draw.stroke()
    }
  }

  def signaled: PartialFunction[(State, Signal), State] = {
    case (Empty, Initialize(settings)) =>
      Running(settings, Seq.empty, Seq.empty)

    case (Running(settings, _, _), Clear) =>
      Running(settings, Seq.empty, Seq.empty)

    case (state: Running, Resize(width, height)) =>
      state.copy(settings = state.settings.copy(width = width, height = height))

    case (Running(Settings(width, height, _, sides), inks, lines), ColorChange(color)) =>
      Running(Settings(width, height, color, sides), inks, lines)

    case (Running(Settings(width, height, color, _), inks, lines), ShapeChange(sides)) =>
      Running(Settings(width, height, color, sides), inks, lines)

    case (Running(settings, inks, lines), MouseDown(touch, x, y)) =>
      Drawing(
        settings,
        Line(
          (if (touch) 3 else 1) * (if (settings.color == "black") 5 else 15),
          settings.color,
          Pt(x-settings.width/2, y-settings.height/2),
          Seq.empty),
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
      Running(settings, inks :+ line, lines)

    case (Drawing(settings, line, inks, lines), MouseUp) =>
      Running(settings, inks, lines :+ line)
      
    case (state, _) => state
  }
  
  def drawLines(settings: Settings, lines: Seq[Line]): Unit = {
    settings.COUNT_LIST.foreach(_ => {
      Fiddle.draw.rotate(settings.ANGLE_DELTA)
      lines.foreach(_.draw)
    })
  }
  
  def render(state: State): Unit = {
    Fiddle.draw.beginPath()
    Fiddle.draw.clearRect(0, 0, state.settings.width, state.settings.height)
    Fiddle.draw.fill()

    Fiddle.draw.save()
    Fiddle.draw.translate(state.settings.width / 2, state.settings.height / 2)

    state match {
      case Running(settings, inks, lines) =>
        drawLines(settings, lines)
        drawLines(settings, inks)

      case Drawing(settings, line, inks, lines) if settings.color == "black" =>
        drawLines(settings, lines)
        drawLines(settings, inks)
        drawLines(settings, Seq(line))

      case Drawing(settings, line, inks, lines) =>
        drawLines(settings, lines)
        drawLines(settings, Seq(line))
        drawLines(settings, inks)

      case _ =>
    }

    Fiddle.draw.restore()

  }

  val machine = Machine
    .onSignal(signaled)
    .onRender(render _)
    .init(Empty)

  def updateCanvasInfo(canvas: dom.html.Canvas): Unit = {
    val width = canvas.clientWidth
    val height = canvas.clientHeight
    canvas.width = width
    canvas.height = height
  }

  def pressEvent(touch: Boolean, x: Double, y: Double): Unit = {
    val cc = Fiddle.canvas.getBoundingClientRect()
    machine.send(MouseDown(touch, x.toInt - cc.left.toInt, y.toInt - cc.top.toInt))
  }

  def moveEvent(x: Double, y: Double): Unit = {
    val cc = Fiddle.canvas.getBoundingClientRect()
    machine.send(MouseMove(x.toInt - cc.left.toInt, y.toInt - cc.top.toInt))
  }

  def releaseEvent(): Unit = {
    machine.send(MouseUp)
  }

  implicit class EventTargetExts(eventTarget: dom.raw.EventTarget) {
    def on[A](event: String, callback: A => _): dom.raw.EventTarget = {
      eventTarget.addEventListener(event, { e: dom.Event =>
        callback(e.asInstanceOf[A])
      })
      eventTarget
    }
  }

  @JSExport
  def main(args: Array[String]): Unit = {

    Fiddle.canvas
      .on("mousedown", { evt: dom.MouseEvent =>
        pressEvent(false, evt.clientX, evt.clientY)
      })
      .on("mousemove", { evt: dom.MouseEvent =>
        moveEvent(evt.clientX, evt.clientY)
      })
      .on("mouseup", { evt: dom.MouseEvent =>
        releaseEvent()
      })
      .on("touchstart", { evt: dom.TouchEvent =>
        val touch = evt.touches(0)
        pressEvent(true, touch.clientX, touch.clientY)
      })
      .on("touchmove", { evt: dom.TouchEvent =>
        val touch = evt.touches(0)
        moveEvent(touch.clientX, touch.clientY)
      })
      .on("touchend", { evt: dom.TouchEvent =>
        releaseEvent()
      })

    dom.window.on("resize", { evt: dom.Event =>
      updateCanvasInfo(Fiddle.canvas)
      machine.send(Resize(Fiddle.canvas.width.toInt, Fiddle.canvas.height.toInt))
    })

    updateCanvasInfo(Fiddle.canvas)

    val colorSelect = dom.document.getElementById("color").asInstanceOf[dom.html.Select]
    colorSelect.on("change", { evt: dom.Event =>
      machine.send(ColorChange(colorSelect.value))
    })

    val shapeSelect = dom.document.getElementById("shape").asInstanceOf[dom.html.Select]
    shapeSelect.on("change", { evt: dom.Event =>
      machine.send(ShapeChange(shapeSelect.value.toInt))
    })

    dom.document.getElementById("clear").asInstanceOf[dom.html.Button]
      .on("click", { evt: dom.Event =>
        machine.send(Clear)
      })

    machine.send(Initialize(Settings(Fiddle.canvas.width, Fiddle.canvas.height, "black", 5)))
  }
}
