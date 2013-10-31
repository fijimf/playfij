package scraping

import models._
import org.joda.time.LocalDate
import models.Season
import models.Result
import models.Team
import models.Game
import models.TeamDao
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import play.api.Logger

trait AbstractGameScraper {
  def logger: Logger

  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  import model.profile.simple._

  private val teamDao: TeamDao = TeamDao(model)
  private val gameDao: GameDao = GameDao(model)

  def dateOk(d: LocalDate, req: GameUpdateRequest, seasons: List[Season]): Boolean = {
    val seasonOk = seasons.foldLeft(false)((b: Boolean, season: Season) => b || season.range.contains(d))
    val requestOk = DateRange(req.fromDate, req.toDate).contains(d)
    seasonOk && requestOk
  }

  def fetch(req: GameUpdateRequest): Future[List[ResultData]]

  def scrape(repo: Repository, req: GameUpdateRequest)(implicit s: scala.slick.session.Session): GameUpdateResult = {
    val seasons = (for (s <- model.Seasons) yield s).list
    val teamsWithAliases: Map[Team, List[String]] = teamDao.listWithAliases
    val candidateData: List[ResultData] = Await.result(fetch(req), 5.minutes)
    val (dateOkData, dateBadData) = candidateData.partition(gd => dateOk(gd.gameData.date, req, seasons))
    val (teamOkData, teamBadData) = mapTeams(dateOkData, teamsWithAliases)
    val res = GameUpdateResult(dateOutsideRange = dateBadData.map(_.gameData), unknownTeam = teamBadData.map(_.gameData))
    val hg = handleGames(req, teamOkData.map(_.gameData), teamsWithAliases.keys.toList, seasons, _: GameUpdateResult)
    val hr = handleResults(req, teamOkData, teamsWithAliases.keys.toList, seasons, _: GameUpdateResult)
    hg.andThen(hr).apply(res)
  }

  def mapTeams(data: List[ResultData], teamsWithAliases: Map[Team, List[String]]): (List[ResultData], List[ResultData]) = {
    val keySet = teamsWithAliases.keys.map(t => t.key).toSet
    val nameMap = teamsWithAliases.keys.map(t => t.name.toLowerCase -> t).toMap
    val aliasMap = teamsWithAliases.keys.foldLeft(Map.empty[String, Team])((map: Map[String, Team], team: Team) => teamsWithAliases(team).foldLeft(map)((m2: Map[String, Team], alias: String) => m2 + (alias.toLowerCase -> team)))
    def lookup(t: String): Option[String] = {
      if (keySet.contains(t)) {
        Some(t)
      } else {
        nameMap.get(t.toLowerCase).orElse(aliasMap.get(t.toLowerCase)).map(_.key)
      }
    }
    val mapped: List[Either[ResultData, ResultData]] = data.map(_.mapTeams(lookup))
    (mapped.filter(_.isRight).map(_.merge), mapped.filter(_.isLeft).map(_.merge))
  }

  def loadCurrentData(req: GameUpdateRequest, seasons: List[Season])(implicit s: scala.slick.session.Session): List[ResultData] = {
    val rows: List[(Game, Team, Team, Option[Int], Option[Int])] = (for (
      (g, r) <- model.Games leftJoin model.Results on (_.id === _.gameId);
      ht <- model.Teams if g.homeTeamId === ht.id;
      at <- model.Teams if g.awayTeamId === at.id
    ) yield (g, ht, at, r.homeScore.?, r.awayScore.?)).list()
    rows.map(_ match {
      case (game, home, away, Some(hs), Some(as)) => {
        ResultData(GameData(game.date, home.key, away.key), Some((hs, as)))
      }
      case (game, home, away, _, _) => {
        ResultData(GameData(game.date, home.key, away.key), None)
      }
    }).filter(rd => dateOk(rd.gameData.date, req, seasons))
  }

  def handleGames(req: GameUpdateRequest, candidateData: List[GameData], teams: List[Team], seasons: List[Season], res: GameUpdateResult)(implicit s: scala.slick.session.Session): GameUpdateResult = {
    val teamKeyMap: Map[String, Team] = teams.map(t => t.key -> t).toMap
    val currentData: List[GameData] = loadCurrentData(req, seasons).map(_.gameData)

    val candidateSet = candidateData.toSet
    val currentSet = currentData.toSet
    logger.info("# Current Games:" + currentSet.size)
    logger.info("# Candidate Games:" + candidateSet.size)

    val inserts = candidateSet.diff(currentSet).toList
    val deletes = currentSet.diff(candidateSet).toList
    val updates = List.empty
    if (req.doGameInserts && req.doWrite) {
      inserts.foreach {
        gd: GameData => {
          val season: Option[Season] = seasons.find(_.range.contains(gd.date))
          model.Games.autoInc.insert(season.get.id, teamKeyMap(gd.home).id, teamKeyMap(gd.away).id, gd.date, None, false)
        }
      }
    }
    if (req.doGameDeletes && req.doWrite) {
      deletes.foreach {
        gd: GameData => {
          val season: Option[Season] = seasons.find(_.range.contains(gd.date))
          model.Games.where(g => (g.seasonId === season.get.id) && (g.homeTeamId === teamKeyMap(gd.home).id) && (g.awayTeamId === teamKeyMap(gd.away).id)).delete
        }
      }
    }

    res.copy(gamesInserted = inserts, gamesUpdated = updates, gamesDeleted = deletes)
  }

  def handleResults(req: GameUpdateRequest, candidateData: List[ResultData], teams: List[Team], seasons: List[Season], res: GameUpdateResult)(implicit s: scala.slick.session.Session): GameUpdateResult = {
    val teamKeyMap: Map[String, Team] = teams.map(t => t.key -> t).toMap
    val currentData: List[ResultData] = loadCurrentData(req, seasons).filter(_.scores.isDefined)

    val candidateMap = candidateData.map(y => y.gameData -> y.scores).toMap
    val currentMap = currentData.map(y => y.gameData -> y.scores).toMap
    logger.info("# Current Results:" + currentMap.size)
    logger.info("# Candidate Results:" + candidateMap.size)


    val inserts = candidateMap.keySet.diff(currentMap.keySet).filter(k => candidateMap(k).isDefined).map(k => ResultData(k, candidateMap(k)))
    val deletes = currentMap.keySet.diff(candidateMap.keySet).filter(k => currentMap(k).isDefined).map(k => ResultData(k, currentMap(k)))
    val updates = currentMap.keySet.intersect(candidateMap.keySet).filter(k => candidateMap(k).isDefined && currentMap(k).isDefined && candidateMap(k) != currentMap(k)).map(k => ResultData(k, candidateMap(k)))
    if (req.doResultInserts && req.doWrite) {
      inserts.foreach {
        rd => {
          val g: Option[Game] = gameDao.findGameBySchedule(rd.gameData.home, rd.gameData.away, rd.gameData.date)
          g.foreach(game => {
            model.Results.autoInc.insert(game.id, rd.scores.get._1, rd.scores.get._2, 0)
          })
        }
      }
    }
    if (req.doResultUpdates && req.doWrite) {
      deletes.foreach {
        rd => {
          val g: Option[Game] = gameDao.findGameBySchedule(rd.gameData.home, rd.gameData.away, rd.gameData.date)
          g.foreach(game => {
            model.Results.where(_.gameId === game.id).update(Result(0, game.id, rd.scores.get._1, rd.scores.get._2, 0))
          })
        }
      }
    }

    if (req.doResultDeletes && req.doWrite) {
      deletes.foreach {
        rd => {
          val g: Option[Game] = gameDao.findGameBySchedule(rd.gameData.home, rd.gameData.away, rd.gameData.date)
          g.foreach(game => {
            model.Results.where(_.gameId === game.id).delete
          })
        }
      }
    }

    res.copy(resultsInserted = inserts.toList, resultsUpdated = updates.toList, resultsDeleted = deletes.toList)

  }
}

object KenPomGameScraper extends AbstractGameScraper {
  def logger = Logger(this.getClass.getName)

  def fetch(req: GameUpdateRequest): Future[List[ResultData]] = {
    PlayGameScraper.scrapeKenPom(req.url)
  }
}

object NcaaGameScraper extends AbstractGameScraper {
  def logger = Logger(this.getClass.getName)

  def fetch(req: GameUpdateRequest): Future[List[ResultData]] = {
    PlayGameScraper.loadGames(req.fromDate.getOrElse(new LocalDate()), req.toDate.getOrElse(new LocalDate().plusMonths(6)))
  }
}
