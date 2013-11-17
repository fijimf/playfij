package models

trait Streak {
  def addWin() : Streak

  def addLoss() : Streak
}
