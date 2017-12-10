package observatory.common

import java.time.Duration

import com.typesafe.scalalogging.LazyLogging

trait Edge {
  def ok: Int
  def fail: Int
  def warn: Int
}

trait TemporalEdge extends Edge {
  def window: Duration
}

object Edge extends LazyLogging {
  final case class Basic(ok: Int, fail: Int, warn: Int) extends Edge
  final case class Temporal(ok: Int, fail: Int, warn: Int, window: Duration) extends TemporalEdge

  def apply(ok: Int, fail: Int, warn: Int): Edge = {
    if (ok >= fail && fail >= warn) {
      Basic(ok, fail, warn)
    } else {
      logger.error(s"ok $ok fail $fail warn $warn")
      throw new RuntimeException(
        s"Invalid config, must have: ok >= fail >= warn, given: ok: $ok fail: $fail warn: $warn")
    }
  }

  def temporal(ok: Int, fail: Int, warn: Int, window: Duration): TemporalEdge = {
    val basic = Basic(ok, fail, warn)
    Temporal(basic.ok, basic.fail, basic.warn, window)
  }
}
