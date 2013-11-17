package models

case class WinStreak(n: Int) extends Streak {
  def addWin : Streak = WinStreak(n + 1)

  def addLoss : Streak = LossStreak(1)

  override def toString: String = "W %d".format(n)
}
