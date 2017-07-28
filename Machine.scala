import rx._
import rx.async._
import rx.async.Platform._
import scala.concurrent.duration._

object Machine {
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
