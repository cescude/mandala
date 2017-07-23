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

  trait State
  case object Empty extends State
  case class Running(color: String, inkLines: Seq[Line], colorLines: Seq[Line]) extends State
  case class Drawing(color: String, line: Line, inkLines: Seq[Line], colorLines: Seq[Line]) extends State

  trait Signal
  case class MouseMove(x: Int, y: Int) extends Signal
  case class MouseDown(x: Int, y: Int) extends Signal
  case class MouseUp(x: Int, y: Int) extends Signal
  case class ColorChange(color: String) extends Signal

  val width = Fiddle.canvas.clientWidth
  val height = Fiddle.canvas.clientHeight
  
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
    case (Running(_, inks, lines), ColorChange(color)) =>
      Running(color, inks, lines)

    case (Running(c, inks, lines), MouseDown(x, y)) =>
      Drawing(c, Line(c, Pt(x-width/2, y-height/2), Seq.empty), inks, lines)

    case (Drawing(c, line, inks, lines), MouseMove(x, y)) =>
      Drawing(c, line.copy(segments = line.segments :+ Pt(x - width/2, y - height/2)), inks, lines)
      
    case (Drawing("black", line, inks, lines), e: MouseUp) =>
      Running("black", inks :+ line, lines)

    case (Drawing(color, line, inks, lines), e: MouseUp) =>
      Running(color, inks, lines :+ line)
      
    case (state, _) => state
  }
  
  def clear[A]: PartialFunction[A, A] = {
    case state =>
      Fiddle.draw.beginPath()
      Fiddle.draw.clearRect(0, 0, width, height)
      Fiddle.draw.fill()
      state
  }
  
  def setup[A]: PartialFunction[A, A] = {
    case state =>
      Fiddle.draw.save()
      Fiddle.draw.translate(width / 2, height / 2)
      state
  }
  
  def teardown: PartialFunction[Unit,Unit] = {
    case state =>
      Fiddle.draw.restore()
      state
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
  
  def render: PartialFunction[State, Unit] = clear[State] andThen setup andThen {
    case Running(_, inks, lines) =>
      drawLines(lines)
      drawLines(inks)

    case Drawing("black", line, inks, lines) =>
      drawLines(lines)
      drawLines(inks)
      drawLines(Seq(line))

    case Drawing(_, line, inks, lines) =>
      drawLines(lines)
      drawLines(Seq(line))
      drawLines(inks)

    case _ =>
  } andThen teardown

  val machine = new Machine[State, Signal](Running("black", Seq.empty, Seq.empty), signaled, render)

  def withMouseEvent(fn: dom.MouseEvent => Unit) = { (evt: dom.Event) =>
    fn(evt.asInstanceOf[dom.MouseEvent])
  }

  @JSExport
  def main(args: Array[String]): Unit = {

    val canvasCoords = Fiddle.canvas.getBoundingClientRect()
    val cleft = canvasCoords.left.toInt
    val ctop  = canvasCoords.top.toInt

    Fiddle.canvas.onmousemove = withMouseEvent { evt =>
      println("onmousemove")
      machine.send(MouseMove(evt.clientX.toInt - cleft, evt.clientY.toInt - ctop))
    }
    
    Fiddle.canvas.onmousedown = withMouseEvent { evt =>
      println("onmouseodwn")
      machine.send(MouseDown(evt.clientX.toInt - cleft, evt.clientY.toInt - ctop))
    }
    
    Fiddle.canvas.onmouseup = withMouseEvent { evt =>
      println("onmouseup")
      machine.send(MouseUp(evt.clientX.toInt - cleft, evt.clientY.toInt - ctop))
    }

    // org.scalajs.dom.ext.TouchEvents.HTMLDocumentToTouchEvents(dom.document).ontouchstart = { evt =>
    //   println(">>>", evt)
    // }

    val colorSelect = dom.document.getElementById("color").asInstanceOf[dom.html.Select]
    colorSelect.onchange = { evt =>
      println("!!!")
      machine.send(ColorChange(colorSelect.value))
    }
  }
}
