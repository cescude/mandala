import rx._
import rx.async._
import rx.async.Platform._
import scala.concurrent.duration._

object Machine {
  def onSignal[St, Sig](handler: PartialFunction[(St, Sig), St]) = Machine1[St,Sig]((st,sig) => handler((st,sig)))
  def onSignal[St, Sig](handler: (St, Sig) => St)                = Machine1[St,Sig](handler)

  case class Machine1[St, Sig](onSignal: (St, Sig) => St) {
    def onRender[World](handler: PartialFunction[(World, St), _]) = Machine2[St,Sig,World](onSignal, (world: World, st: St) => handler((world,st)))
    def onRender[World](handler: (World, St) => _)                = Machine2[St,Sig,World](onSignal, handler)
  }

  case class Machine2[St, Sig, World](onSignal: (St, Sig) => St, onRender: (World, St) => _) {
    def init(world: World, initialState: St)(implicit ctx: Ctx.Owner) =
      Machine(world, initialState, onSignal, onRender)(ctx)
  }
}

case class Machine[St, Sig, World](
  world: World,
  init: St,
  onSignal: (St, Sig) => St,
  onRender: (World, St) => _)(implicit val ctx: Ctx.Owner) {

  val state: Var[St] = Var(init)
  val obs = state.debounce((1000/30).millis).trigger {
    onRender(world, state.now)
  }
  
  def send(signal: Sig): Unit = {
    state() = onSignal(state.now, signal)
  }
}
