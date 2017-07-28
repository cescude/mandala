object Fiddle {
  import org.scalajs.dom

  val canvas = dom.document.getElementById("fiddle-canvas")
    .asInstanceOf[dom.html.Canvas]
  val draw = canvas.getContext("2d")
}
