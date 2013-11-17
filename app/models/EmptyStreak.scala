package models

case object EmptyStreak extends Streak {
  def addWin() : Streak = WinStreak(1)

  def addLoss() : Streak = LossStreak(1)

  override def toString: String = "-"
}
