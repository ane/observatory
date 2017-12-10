package observatory.fsm

import java.time.Duration

import observatory.common._
import cats.implicits._
import cats.data._

import scala.collection.immutable.Queue

object EdgeFSM {
  type Result[A] = State[EdgeState, A]

  def observationMin(state: EdgeState): Int = state.edge.ok + state.edge.fail + state.edge.warn

  def partition: Result[(Int, Int)] =
    State.get[EdgeState].map { state =>
      state.observations.foldLeft((0, 0)) {
        case ((succ, fail), event) =>
          event match {
            case qual: Event.Qualitative => if (qual.success) (succ + 1, fail) else (succ, fail + 1)
            case temp: Event.Temporal =>
              state.edge match {
                case sap: Edge.Basic => (succ + 1, fail)
                case edge: Edge.Temporal =>
                  if (temp.duration.compareTo(edge.window) > 0) (succ + 1, fail) else (succ, fail + 1)
              }
          }
      }
    }

  def report: Result[EdgeStatus] = {
    for {
      state      <- State.get[EdgeState]
      partitions <- partition
    } yield {
      val (succeeded, failed) = partitions
      val observations        = state.observations
      val edge                = state.edge
      val needed              = observationMin(state)
      if (observations.lengthCompare(needed) < 0) {
        Uncertain
      } else {
        if (failed >= edge.fail) {
          Fail
        } else if (failed >= edge.warn) {
          Warn
        } else if (succeeded >= edge.ok) {
          OK
        } else {
          Uncertain
        }
      }
    }
  }

  def store(event: Event): Result[Unit] =
    State.modify[EdgeState] { state =>
      val observations =
        if (state.observations.lengthCompare(observationMin(state)) >= 0)
          state.observations.dequeue._2.enqueue(event)
        else state.observations.enqueue(event)
      state.copy(observations = observations)
    }

  case class EdgeState(status: EdgeStatus, observations: Queue[Event], edge: Edge)

  object EdgeState {
    def init(edge: Edge): EdgeState = EdgeState(Uncertain, Queue.empty, edge)
  }

  sealed trait EdgeStatus
  case object Uncertain extends EdgeStatus
  case object OK        extends EdgeStatus
  case object Warn      extends EdgeStatus
  case object Fail      extends EdgeStatus

  sealed trait Input
  case class EdgeEvent(event: Event) extends Input
  case object Report                 extends Input
}
