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
    def onRender[Env](handler: PartialFunction[(Env, St), _]) =
      Machine2[St,Sig,Env](onSignal, (env: Env, st: St) => handler((env,st)))

    def onRender[Env](handler: (Env, St) => _) =
      Machine2[St,Sig,Env](onSignal, handler)
  }

  case class Machine2[St, Sig, Env](onSignal: (Sig, St) => St, onRender: (Env, St) => _) {
    def init(env: Env, initialState: St)(implicit ctx: Ctx.Owner) =
      Machine(env, initialState, onSignal, onRender)(ctx)
  }
}

case class Machine[St, Sig, Env](
  env: Env,
  init: St,
  onSignal: (Sig, St) => St,
  onRender: (Env, St) => _)(implicit val ctx: Ctx.Owner) {

  val state: Var[St] = Var(init)
  val obs = state.debounce((1000/30).millis).trigger {
    onRender(env, state.now)
  }
  
  def send(signal: Sig): Unit = {
    state() = onSignal(signal, state.now)
  }
}
