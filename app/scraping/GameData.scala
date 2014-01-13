package scraping

import org.joda.time.LocalDate
import scala.util.{Failure, Try, Success}

case class GameData(date:LocalDate,home:String, away:String) {

  override def toString:String = {
    "%14s %24s %24s".format(date.toString("yyyy-MM-dd"), home, away)
  }
  def mapTeams(f:(String)=>Option[String]):Either[GameData,GameData] = {
    (f(home), f(away)) match {
      case (Some(h), Some(a)) => Right(GameData(date, h, a))
      case (None, Some(a)) => Left(GameData(date, home, ""))
      case (Some(h), None) => Left(GameData(date, "", away))
      case (None, None) => Left(this)
    }
  }





}
