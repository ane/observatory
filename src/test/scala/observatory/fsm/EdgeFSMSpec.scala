package observatory.fsm

import java.time.ZonedDateTime
import java.util.UUID

import cats.Eval
import observatory.common.{Edge, Event, Node}
import cats.data._
import cats.implicits._
import observatory.fsm.EdgeFSM.{EdgeEvent, EdgeState, Report}
import org.scalatest._

class EdgeFSMSpec extends WordSpec with Matchers {
  val edge               = Edge.Basic(5, 4, 3)
  val src                = Node("fib", UUID.randomUUID())
  val dst                = Node("fab", UUID.randomUUID())
  val now: ZonedDateTime = ZonedDateTime.now()
  val data = List(
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, false, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, false, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now),
    Event.Qualitative(src, dst, true, now)
  )

  "EdgeFSM" should {
    "work correctly" in {
      val s               = data.traverse_(EdgeFSM.store(_)).flatMap(_ => EdgeFSM.report)
      val initialState    = EdgeState.init(edge)
      val (state, status) = s.run(initialState).value
      state.observations.length shouldBe 12
      status shouldBe EdgeFSM.OK
    }
  }
}
