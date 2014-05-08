package orderbook

import models.{User, Game}

case class Order(game: Game, user: User, side: Side, amount: Int, level: Double)
