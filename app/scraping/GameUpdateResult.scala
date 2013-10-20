package scraping

import org.apache.commons.lang3.StringUtils

case class GameUpdateResult(
                             unknownTeam: List[GameData] = List.empty[GameData],
                             dateOutsideRange: List[GameData] = List.empty[GameData],
                             gamesInserted: List[GameData] = List.empty[GameData],
                             gamesUpdated: List[GameData] = List.empty[GameData],
                             gamesDeleted: List[GameData] = List.empty[GameData],
                             resultsInserted: List[ResultData] = List.empty[ResultData],
                              resultsUpdated: List[ResultData] = List.empty[ResultData],
                             resultsDeleted: List[ResultData] = List.empty[ResultData]) {
  def unmappedTeams(filter:Int=3):List[(String, Int)] = {
    val names: List[String] = (unknownTeam.map(_.home) ++ unknownTeam.map(_.away)).filter(t => StringUtils.isNotBlank(t))
    val count=names.foldLeft(Map.empty[String, Int])((map: Map[String, Int], s: String) =>map+(s->(map.getOrElse(s,0)+1)))
    count.toList.sortBy(_._2).reverse.filter(_._2>filter)
  }
}


