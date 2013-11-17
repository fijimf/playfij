package models

/**
 * Created with IntelliJ IDEA.
 * User: Jim
 * Date: 11/15/13
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */
case class WinStreak(n: Int) extends Streak {
  def addWin : Streak = WinStreak(n + 1)

  def addLoss : Streak = LossStreak(1)
}
