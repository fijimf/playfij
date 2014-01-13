package scraping

case class ResultData(gameData:GameData, scores:Option[(Int, Int)]) {

  override def toString:String = {
    "%14s %24s %3d %24s %3d ".format(gameData.date.toString("yyyy-MM-dd"), gameData.home, scores.map(_._1).getOrElse(0), gameData.away, scores.map(_._2).getOrElse(0))
  }

  def mapTeams(f:(String)=>Option[String]):Either[ResultData,ResultData] = {
    gameData.mapTeams(f) match {
      case Left(gd)=> Left(ResultData(gd, scores))
      case Right(gd)=> Right(ResultData(gd, scores))
    }
  }
}
