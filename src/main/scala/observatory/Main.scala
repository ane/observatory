package observatory

import fs2._
import java.time.{Duration, Period, ZonedDateTime}
import java.util.UUID

import scala.annotation.tailrec

case class Event(timestamp: ZonedDateTime, id: UUID)

case class Node(id: String, history: Seq[Event])

sealed trait Status
case object OK   extends Status
case object NOK  extends Status
case object WARN extends Status

trait Checker {
  def run(a: Node)(b: Node): Option[Status]
}

case class Edge(src: String, dst: String)

case class Temporal(window: Int, ok: Int, warn: Int, fail: Int) extends Checker {
  override def run(a: Node)(b: Node): Option[Status] = {
    val pairs =
      for (e1 <- a.history; e2 <- b.history; if e1.id == e2.id)
        yield (e1, e2)

    Some(pairs).filter(_.nonEmpty).map { p =>
      p span {
        case (e1, e2) =>
          Duration.between(e1.timestamp, e2.timestamp).toMillis <= window
      }
    } flatMap {
      case (successes, failures) =>
        if (failures.length >= fail) Some(NOK)
        else if (failures.length >= warn) Some(WARN)
        else if (successes.length >= ok) Some(OK)
        else None
    }
  }
}

case class Listener(src: Node, dst: Node) {
  case class ListenerState(history: List[String])

  def start[F[_], R](h: Handle[F, String]): Pull[F, ListenerState, R] = {
    val initial = ListenerState(List.empty)
    run(initial)(h)
  }

  private def run[F[_], R](old: ListenerState)(h: Handle[F, String]): Pull[F, ListenerState, R] =
    h.await1.flatMap {
      case (data, h) =>
        val filtered = if (data == src.id || data == dst.id) List(data) else List.empty
        val newState = old.copy(history = old.history ++ filtered)
        Pull.output(Chunk.singleton(newState)) >>
          run(old.copy(history = old.history ++ filtered))(h)
    }
}

object Main {
  def edgeListener(e: Edge) = {}
}
