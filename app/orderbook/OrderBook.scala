package orderbook

import models.{User, Game}
import org.joda.time.LocalDateTime
import java.util.UUID

case class OrderBook(game: Game, orders: List[(Order, UUID, LocalDateTime)]) {
  def submit(order: Order): (OrderConfirm, OrderBook) = {
    val id = UUID.randomUUID()
    val timestamp: LocalDateTime = new LocalDateTime()
    (Accepted(id, timestamp), copy(orders = (order, id, timestamp) :: orders))
  }

  def cancel(id:UUID):(OrderConfirm, OrderBook) = {

    val timestamp: LocalDateTime = new LocalDateTime()
    Cancelled(id, timestamp)
  }

  def marketDepth(): List[(Double, Double, Double)] = {
    val levels: Map[Double, List[Order]] = orders.map(_._1).groupBy(_.level)
    val sizes: Map[Double, (Int, Int)] = levels.mapValues(lst => (lst.filter(_.side == Home).map(_.amount).sum, lst.filter(_.side == Away).map(_.amount).sum))
    sizes.toList.map(x => (x._1, x._2._1.toDouble, x._2._2.toDouble))

  }
}



