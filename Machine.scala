import rx._
import rx.async._
import rx.async.Platform._
import scala.concurrent.duration._

object Machine {
  def onSignal[Sig, St](handler: PartialFunction[(Sig, St), St]) =
    Machine1[St,Sig]((sig,st) => handler((sig,st)))

  def onSignal[Sig, St](handler: (Sig, St) => St) =
    Machine1[St,Sig](handler)

  case class Machine1[St, Sig](onSignal: (Sig, St) => St) {
    def onRender(handler: PartialFunction[St, _]) =
      Machine2[St,Sig](onSignal, (st: St) => handler(st))

    def onRender[Env](handler: St => _) =
      Machine2[St,Sig](onSignal, handler)
  }

  case class Machine2[St, Sig](onSignal: (Sig, St) => St, onRender: St => _) {
    def init(initialState: St)(implicit ctx: Ctx.Owner) =
      Machine(initialState, onSignal, onRender)(ctx)
  }
}

case class Machine[St, Sig](
  init: St,
  onSignal: (Sig, St) => St,
  onRender: St => _)(implicit val ctx: Ctx.Owner) {

  val state: Var[St] = Var(init)
  val obs = state.debounce((1000/30).millis).trigger {
    onRender(state.now)
  }
  
  def send(signal: Sig): Unit = {
    state() = onSignal(signal, state.now)
  }
}
