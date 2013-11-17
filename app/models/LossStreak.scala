package models

case class LossStreak(n: Int) extends Streak {
  def addWin() : Streak = WinStreak(1)

  def addLoss() : Streak = LossStreak(n + 1)
}
