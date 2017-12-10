package observatory.common

import java.time.{Duration, ZonedDateTime}

trait Event {
  def source: Node
  def target: Node
}

object Event {
  final case class Qualitative(source: Node, target: Node, success: Boolean, timestamp: ZonedDateTime) extends Event

  final case class Temporal(source: Node, target: Node, start: ZonedDateTime, end: ZonedDateTime) extends Event {
    def duration: Duration = Duration.between(start, end)
  }
}
