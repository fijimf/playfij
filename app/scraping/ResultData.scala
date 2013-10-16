package scraping

case class ResultData(gameData:GameData, scores:Option[(Int, Int)]) {
  def mapTeams(f:(String)=>Option[String]):Either[ResultData,ResultData] = {
    gameData.mapTeams(f) match {
      case Left(gd)=> Left(ResultData(gd, scores))
      case Right(gd)=> Right(ResultData(gd, scores))
    }
  }
}
