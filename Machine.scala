import rx._
import rx.async._
import rx.async.Platform._
import scala.concurrent.duration._

object Machine {
  def onSignal[St, Sig](handler: PartialFunction[(St, Sig), St]) =
    Machine1[St,Sig]((st,sig) => handler((st,sig)))

  def onSignal[St, Sig](handler: (St, Sig) => St) =
    Machine1[St,Sig](handler)

  case class Machine1[St, Sig](onSignal: (St, Sig) => St) {
    def onRender[Env](handler: PartialFunction[(Env, St), _]) =
      Machine2[St,Sig,Env](onSignal, (env: Env, st: St) => handler((env,st)))

    def onRender[Env](handler: (Env, St) => _) =
      Machine2[St,Sig,Env](onSignal, handler)
  }

  case class Machine2[St, Sig, Env](onSignal: (St, Sig) => St, onRender: (Env, St) => _) {
    def init(env: Env, initialState: St)(implicit ctx: Ctx.Owner) =
      Machine(env, initialState, onSignal, onRender)(ctx)
  }
}

case class Machine[St, Sig, Env](
  env: Env,
  init: St,
  onSignal: (St, Sig) => St,
  onRender: (Env, St) => _)(implicit val ctx: Ctx.Owner) {

  val state: Var[St] = Var(init)
  val obs = state.debounce((1000/30).millis).trigger {
    onRender(env, state.now)
  }
  
  def send(signal: Sig): Unit = {
    state() = onSignal(state.now, signal)
  }
}
