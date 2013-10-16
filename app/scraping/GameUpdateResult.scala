package scraping

case class GameUpdateResult(
                             unknownTeam: List[GameData] = List.empty[GameData],
                             dateOutsideRange: List[GameData] = List.empty[GameData],
                             gamesInserted: List[GameData] = List.empty[GameData],
                             gamesUpdated: List[GameData] = List.empty[GameData],
                             gamesDeleted: List[GameData] = List.empty[GameData],
                             resultsInserted: List[ResultData] = List.empty[ResultData],
                             resultsUpdated: List[ResultData] = List.empty[ResultData],
                             resultsDeleted: List[ResultData] = List.empty[ResultData])


