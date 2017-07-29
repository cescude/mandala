import scalatags.JsDom.all._
import org.scalajs.dom
import scala.scalajs.js.annotation._
import Mandala._

// This file boots & configures the html machine, providing signals in, and a
// canvas instance for output.

@JSExportTopLevel("mandala")
object Main {
  val logic = Mandala(Fiddle.draw.asInstanceOf[dom.CanvasRenderingContext2D])

  val machine = Machine
    .onSignal(logic.signaled)
    .onRender(logic.render(_))
    .init(World(Settings(0, 0, "black", 7), Paused(Seq.empty, Seq.empty)))

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
    def on[A](event: String, preventDefault: Boolean, callback: A => _): dom.raw.EventTarget = {
      eventTarget.addEventListener(event, { e: dom.Event =>
        callback(e.asInstanceOf[A])
        if (preventDefault) e.preventDefault()
      })
      eventTarget
    }

    def on[A](event: String, callback: A => _): dom.raw.EventTarget =
      on(event, false, callback)
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
      .on("touchstart", true, { evt: dom.TouchEvent =>
        val touch = evt.touches(0)
        pressEvent(true, touch.clientX, touch.clientY)
      })
      .on("touchmove", true, { evt: dom.TouchEvent =>
        val touch = evt.touches(0)
        moveEvent(touch.clientX, touch.clientY)
      })
      .on("touchend", true, { evt: dom.TouchEvent =>
        releaseEvent()
      })

    dom.window.on("resize", { evt: dom.Event =>
      updateCanvasInfo(Fiddle.canvas)
      machine.send(Resize(Fiddle.canvas.width.toInt, Fiddle.canvas.height.toInt))
    })

    updateCanvasInfo(Fiddle.canvas)

    val controls = dom.document.getElementById("controls")
    import scalatags.JsDom._

    val clearButton = button(i(cls := "fa fa-trash-o")).render
    clearButton.on("click", { evt: dom.Event => machine.send(Clear) })
    controls.appendChild(clearButton)

    val shapeSelect = select().render
    shapeSelect.on("change", { evt: dom.Event =>
      machine.send(ShapeChange(shapeSelect.value.toInt))
    })
    controls.appendChild(shapeSelect)

    "Digon Triangle Square Pentagon Hexagon Heptagon Octagon Nonagon Decagon Hendecagon Dodecagon"
      .split(" ").zipWithIndex
      .map({ case (name, index) => option(value := s"${index+2}", name) })
      .map(_.render)
      .map(shapeSelect.appendChild(_))

    shapeSelect.value = "7"

    val black = Seq("black")
    val hsls = Range(0, 360, 360/16).map(hue => s"hsl($hue, 100%, 50%)").take(14)

    (black ++ hsls)
      .map({ colorName =>
        val btn = button(i(cls := "fa fa-square fa-2x", color := colorName)).render
        btn.on("click", { evt : dom.Event => machine.send(ColorChange(colorName)) })
        btn
      })
      .foreach(controls.appendChild(_))

    machine.send(Initialize(Settings(Fiddle.canvas.width, Fiddle.canvas.height, "black", 7)))
  }
}
