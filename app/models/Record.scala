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

}
