package observatory

import java.time.{Duration, Period, ZonedDateTime}
import java.util.UUID

case class Event(timestamp: ZonedDateTime, id: UUID)

case class Node(id: String, history: Seq[Event])

sealed trait Status
case object OK extends Status
case object NOK extends Status
case object WARN extends Status

trait Checker {
  def run(a: Node)(b: Node): Option[Status]
}

case class Temporal(window: Int, ok: Int, warn: Int, fail: Int)
    extends Checker {
  override def run(a: Node)(b: Node): Option[Status] = {
    val pairs =
      for {
        e1 <- a.history
        e2 <- b.history
        if e1.id == e2.id
      } yield {
        (e1, e2)
      }

    val results =
      for {
        p <- Some(pairs) if pairs.nonEmpty
      } yield {
        p.span {
          case (e1, e2) =>
            Duration.between(e1.timestamp, e2.timestamp).toMillis <= window
        }
      }

    results flatMap { case (successes, failures) =>
      if (failures.length >= fail) Some(NOK)
      else if (failures.length >= warn) Some(WARN)
      else if (successes.length >= ok) Some(OK)
      else None
    }
  }
}

object Main {}
