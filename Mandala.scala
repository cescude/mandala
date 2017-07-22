import scala.scalajs.js.annotation._
import rx._

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
  val obs = state.trigger {
    onRender(state.now)
  }
  
  def send(signal: Sig): Unit = {
    state() = onSignal(state.now, signal)
  }
}

@JSExportTopLevel("mandala")
object Mandala {

  import scala.scalajs.js.timers._
  import scalatags.JsDom.all._
  import org.scalajs.dom
  
  val NUM_SIDES = 7

  trait State
  case object Empty extends State
  case class Running(color: String, ops: Seq[Op]) extends State
  case class Drawing(color: String, ops: Seq[Op]) extends State

  trait Signal
  case object Tick extends Signal
  case class MouseMove(x: Int, y: Int) extends Signal
  case class MouseDown(x: Int, y: Int) extends Signal
  case class MouseUp(x: Int, y: Int) extends Signal
  case class ColorChange(color: String) extends Signal
  
  val width = Fiddle.canvas.clientWidth
  val height = Fiddle.canvas.clientHeight
  
  sealed trait Op {
    def x: Int
    def y: Int
  }
  case class Start(color: String, x: Int, y: Int) extends Op
  case class Line(x: Int, y: Int) extends Op
  
  def drawit(ops: Seq[Op]): Unit = {
    Fiddle.draw.beginPath()
    ops foreach {
      case Start(color, x, y) =>
        Fiddle.draw.stroke()
        Fiddle.draw.beginPath()
        Fiddle.draw.strokeStyle = color;
        Fiddle.draw.lineWidth = 5
        Fiddle.draw.moveTo(x, y)
        
      case Line(x, y) => Fiddle.draw.lineTo(x, y)
    }
    Fiddle.draw.stroke()
  }
  
  def signaled: PartialFunction[(State, Signal), State] = {
    case (Running(_, ops), ColorChange(color)) =>
      Running(color, ops)

    case (Running(c, ops), MouseDown(x, y)) =>
      Drawing(c, ops :+ Start(c, x - width/2, y - height/2))

    case (Drawing(_, ops), ColorChange(color)) =>
      Drawing(color, ops)

    case (Drawing(c, ops), MouseMove(x, y)) =>
      Drawing(c, ops :+ Line(x - width/2, y - height/2))
      
    case (Drawing(c, ops), MouseUp(_, _)) =>
      Running(c, ops)
      
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
  
  def drawLines(ops: Seq[Op]): Unit = {
    val angle = 2 * Math.PI / NUM_SIDES

    Range(0, NUM_SIDES).foreach { i =>
      Fiddle.draw.rotate(angle)
      drawit(ops)
    }
  }
  
  def render: PartialFunction[State, Unit] = clear[State] andThen setup andThen {
    case Running(_, ops) =>
      drawLines(ops)

    case Drawing(_, ops) =>
      drawLines(ops)

    case _ =>
  } andThen teardown

  val machine = new Machine[State, Signal](Running("black", Seq.empty), signaled, render)

  def withMouseEvent(fn: dom.MouseEvent => Unit) = { (evt: dom.Event) =>
    fn(evt.asInstanceOf[dom.MouseEvent])
  }

  @JSExport
  def main(args: Array[String]): Unit = {
    setInterval(1000 / 30) {
      machine.send(Tick)
    }

    val canvasCoords = Fiddle.canvas.getBoundingClientRect()
    val cleft = canvasCoords.left.toInt
    val ctop  = canvasCoords.top.toInt
    
    Fiddle.canvas.onmousemove = withMouseEvent { evt =>
      machine.send(MouseMove(evt.clientX.toInt - cleft, evt.clientY.toInt - ctop))
    }
    
    Fiddle.canvas.onmousedown = withMouseEvent { evt =>
      machine.send(MouseDown(evt.clientX.toInt - cleft, evt.clientY.toInt - ctop))
    }
    
    Fiddle.canvas.onmouseup = withMouseEvent { evt =>
      machine.send(MouseUp(evt.clientX.toInt - cleft, evt.clientY.toInt - ctop))
    }

    val colorSelect = dom.document.getElementById("color").asInstanceOf[dom.html.Select]
    colorSelect.onchange = { evt =>
      machine.send(ColorChange(colorSelect.value))
    }
  }
}
