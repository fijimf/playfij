package orderbook

sealed trait Side

case object Home extends Side

case object Away extends Side

