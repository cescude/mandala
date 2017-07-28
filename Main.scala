import scalatags.JsDom.all._
import org.scalajs.dom
import scala.scalajs.js.annotation._
import Mandala._

// This file boots & configures the html machine, providing signals in, and a
// canvas instance for output.

@JSExportTopLevel("mandala")
object Main {
  val machine = Machine
    .onSignal(signaled)
    .onRender(render _)
    .init(
      Fiddle.draw.asInstanceOf[dom.CanvasRenderingContext2D],
      Paused(
        Settings(Fiddle.canvas.width, Fiddle.canvas.height, "black", 7),
        Seq.empty,
        Seq.empty))

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
        evt.preventDefault()
      })
      .on("touchend", { evt: dom.TouchEvent =>
        releaseEvent()
      })

    dom.window.on("resize", { evt: dom.Event =>
      updateCanvasInfo(Fiddle.canvas)
      machine.send(Resize(Fiddle.canvas.width.toInt, Fiddle.canvas.height.toInt))
    })

    updateCanvasInfo(Fiddle.canvas)

    val controls = dom.document.getElementById("controls")
    import scalatags.JsDom._

    val clearButton = button(width := "48px", height := "48px", "Clear").render
    clearButton.on("click", { evt: dom.Event => machine.send(Clear) })
    controls.appendChild(clearButton)

    val shapeSelect = select(height := "48px").render
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
    val hsls = Range(0, 360, 360/16).map(hue => s"hsl($hue, 100%, 70%)").take(14)

    (black ++ hsls)
      .map({ colorName =>
        val btn = button("//",
          backgroundColor := colorName, color := colorName,
          width := "48px", height := "48px").render
        btn.on("click", { evt : dom.Event => machine.send(ColorChange(colorName)) })
        btn
      })
      .foreach(controls.appendChild(_))

    machine.send(Initialize(Settings(Fiddle.canvas.width, Fiddle.canvas.height, "black", 7)))
  }
}
