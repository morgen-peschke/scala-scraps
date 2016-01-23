package com.peschke.graphics.utils

import akka.actor.{ ActorRef, FSM }
import scala.concurrent.duration._

object Timer {
  sealed trait State
  object State {
    private [Timer] case object Pending   extends State
    private [Timer] case object Running   extends State
    private [Timer] case object Paused    extends State
    private [Timer] case object Stopped   extends State
  }

  sealed trait Msg
  object Msg {
    case object Start extends Msg
    case object Pause extends Msg
    case object Stop  extends Msg
    private [Timer] case object Trigger extends Msg
  }

  sealed trait Action[+T]
  object Action {
    case object Stop extends Action[Nothing]
    case class  Next[T](data: T, nextWait: Option[FiniteDuration] = None) extends Action[T]
  }
}

class Timer[V](defaultInterval: FiniteDuration,
               firstValue: => V,
               eachTime: V => Timer.Action[V]) extends FSM[Timer.State,V] {

  import Timer._

  startWith(State.Pending, firstValue)

  when(State.Pending) {
    case Event(Msg.Start, _) => goto (State.Running)
    case Event(Msg.Stop,  _) => goto (State.Stopped)
  }

  when (State.Running) {
    case Event(Msg.Pause, _) => goto (State.Paused)
    case Event(Msg.Stop,  _) => goto (State.Stopped)
    case Event(Msg.Trigger, stateDate) =>
      eachTime(stateDate) match {
        case Action.Stop => goto (State.Stopped)
        case Action.Next(data, interval) =>
          startTimer(interval getOrElse defaultInterval)
          goto (State.Running) using (data)
      }
  }

  when (State.Paused) {
    case Event(Msg.Pause, _) => goto (State.Running)
    case Event(Msg.Stop, _) => goto (State.Stopped)
  }

  when (State.Stopped) {
    case Event(_, _) => stop()
  }

  onTransition {
    case State.Pending -> State.Running => startTimer(defaultInterval)
    case State.Paused  -> State.Running => startTimer(defaultInterval)
    case _ -> State.Paused  => stopTimer()
    case _ -> State.Stopped =>
      stopTimer()
      stop()
  }

  private def startTimer(interval: FiniteDuration) =
    setTimer("tickTimer", Msg.Trigger, interval)

  private def stopTimer()  = cancelTimer("tickTimer")

  initialize()
}
