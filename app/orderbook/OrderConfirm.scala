package orderbook

import org.joda.time.LocalDateTime
import java.util.UUID

sealed trait OrderConfirm {
  def timestamp: LocalDateTime
}

case class Accepted(id: UUID, timestamp: LocalDateTime) extends OrderConfirm

case class Cancelled(id: UUID, timestamp: LocalDateTime) extends OrderConfirm




