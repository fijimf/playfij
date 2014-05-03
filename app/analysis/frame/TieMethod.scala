package analysis.frame

sealed trait TieMethod

case object HighRank extends TieMethod

case object LowRank extends TieMethod

case object Average extends TieMethod


