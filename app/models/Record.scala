package models

case class Record(wins: Int = 0, losses: Int = 0, streak: Streak = EmptyStreak) extends Comparable[Record] {
  def addWin(): Record = Record(wins + 1, losses, streak.addWin())

  def addLoss(): Record = Record(wins, losses + 1, streak.addLoss())

  def compareTo(r: Record): Int = {
    val d = (wins - losses) - (r.wins - r.losses)
    if (d == 0) {
      wins - r.wins
    } else {
      d
    }
  }

  override def toString: String = {
    "%d - %d".format(wins, losses)
  }

  val pct:Option[Double] = if (wins + losses > 0) {
    Some(wins.toDouble / (wins.toDouble + losses.toDouble))
  } else {
    None
  }

  def pctString(fmt: String = "%06.4f"):String = {
    pct match {
      case Some(p) => fmt.format(p)
      case _ => "-"
    }
  }

  def performance(cmp: Record):Option[Double] = {
     (pct, cmp.pct) match {
      case (Some(p), Some(q)) => Some(p - q)
      case _ => None
    }
  }

  def performanceString(cmp: Record, fmt: String = "%+06.4f"):String = {
    performance(cmp) match {
      case Some(p) => fmt.format(p)
      case None => "-"
    }
  }
}
