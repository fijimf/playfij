package scraping

import models._
import org.joda.time.LocalDate
import models.Season
import models.Result
import models.Team
import models.Game
import models.TeamDao
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.Logger

object KenPomGameScraper {
  val logger = Logger(this.getClass.getName)

    import play.api.Play.current

    private val model = new Model() {
      val profile = play.api.db.slick.DB.driver
    }

    import model.profile.simple._

    private val teamDao: TeamDao = TeamDao(model)

    def dateOk(d: LocalDate, req: GameUpdateRequest, seasons:List[Season]): Boolean = {
      val seasonOk = seasons.foldLeft(false)((b: Boolean, season: Season) => b || season.range.contains(d))
      val requestOk = DateRange(req.fromDate, req.toDate).contains(d)
      seasonOk && requestOk
    }

    def scrape(repo: Repository, req: GameUpdateRequest)(implicit s: scala.slick.session.Session): GameUpdateResult = {
      val seasons = (for (s <- model.Seasons) yield s).list
      val teamsWithAliases: Map[Team, List[String]] = teamDao.listWithAliases


      val candidateData: List[ResultData] = Await.result(PlayGameScraper.scrapeKenPom(req.url), 2.minutes)
      val (dateOkData, dateBadData) = candidateData.partition(gd=> dateOk(gd.gameData.date, req, seasons))
      val (teamOkData, teamBadData) = mapTeams(dateOkData, teamsWithAliases)
      val res = GameUpdateResult(dateOutsideRange = dateBadData.map(_.gameData), unknownTeam = teamBadData.map(_.gameData))
      val hg = handleGames(req, teamOkData.map(_.gameData), teamsWithAliases.keys.toList, seasons, _:GameUpdateResult)
      val hr = handleResults(req, teamOkData, seasons, _:GameUpdateResult)
      hg.andThen(hr).apply(res)
    }

    def mapTeams(data: List[ResultData], teamsWithAliases: Map[Team, List[String]]): (List[ResultData], List[ResultData]) = {
      val teamMap = teamsWithAliases.keys.map(t => t.name.toLowerCase -> t).toMap
      val aliasMap = teamsWithAliases.keys.foldLeft(Map.empty[String, Team])((map: Map[String, Team], team: Team) => teamsWithAliases(team).foldLeft(map)((m2: Map[String, Team], alias: String) => m2 + (alias.toLowerCase -> team)))
      def lookup(t: String): Option[String] = teamMap.get(t.toLowerCase).orElse(aliasMap.get(t.toLowerCase)).map(_.key)
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
        case (game, home, away, Some(hs), Some(as)) => { ResultData(GameData(game.date, home.key, away.key), Some((hs, as)))}
        case (game, home, away, _,_) => { ResultData(GameData(game.date, home.key, away.key),None)}
      }).filter(rd => dateOk(rd.gameData.date, req, seasons))
    }

    def handleGames(req: GameUpdateRequest, candidateData: List[GameData], teams:List[Team], seasons:List[Season], res: GameUpdateResult)(implicit s: scala.slick.session.Session): GameUpdateResult = {
      val teamKeyMap: Map[String, Team] = teams.map(t=>t.key->t).toMap
      val currentData: List[GameData] = loadCurrentData(req, seasons).map(_.gameData)

      val candidateSet = candidateData.toSet
      val currentSet = currentData.toSet
      logger.info("# Current Games:"+currentSet.size)
      logger.info("# Candidate Games:"+candidateSet.size)

      val inserts = candidateSet.diff(currentSet).toList
      val deletes = currentSet.diff(candidateSet).toList
      val updates = List.empty
      if (req.doGameInserts && req.doWrite) {
        inserts.foreach {
          gd:GameData => {
            val season: Option[Season] = seasons.find(_.range.contains(gd.date))
            model.Games.autoInc.insert(season.get.id, teamKeyMap(gd.home).id, teamKeyMap(gd.away).id, gd.date, None, false)
          }
        }
      }
      if (req.doGameDeletes && req.doWrite) {
        deletes.foreach {
          gd:GameData => {
            val season: Option[Season] = seasons.find(_.range.contains(gd.date))
            model.Games.where(g=>(g.seasonId === season.get.id) && (g.homeTeamId === teamKeyMap(gd.home).id) && (g.awayTeamId === teamKeyMap(gd.away).id)).delete
          }
        }
      }

      res.copy( gamesInserted = inserts, gamesUpdated = updates, gamesDeleted = deletes)
    }

    def handleResults(req: GameUpdateRequest, candidateData: List[ResultData], teams:List[Team], seasons:List[Season], res: GameUpdateResult)(implicit s: scala.slick.session.Session): GameUpdateResult = {
      val teamKeyMap: Map[String, Team] = teams.map(t=>t.key->t).toMap
      val currentData: List[ResultData] = loadCurrentData(req, seasons)

      val candidateMap = candidateData.map(y => y.gameData -> y.scores).toMap
      val currentMap = currentData.map(y => y.gameData -> y.scores).toMap

      val keys = currentMap.keySet.intersect(candidateMap.keySet)

      val inserts = keys.filter(k=>candidateMap(k).isDefined && currentMap(k).isEmpty).map(k=>ResultData(k, candidateMap(k)))
      val deletes = keys.filter(k=>candidateMap(k).isEmpty && currentMap(k).isDefined).map(k=>ResultData(k, currentMap(k)))
      val updates = keys.filter(k=>candidateMap(k).isDefined && currentMap(k).isDefined && candidateMap(k) != currentMap(k)).map(k=>ResultData(k, candidateMap(k)))
      if (req.doResultInserts) {
        inserts.foreach {
          rd => {
            for(g <- model.Games if (g.homeTeamId === teamKeyMap(rd.gameData.home).id && g.awayTeamId === teamKeyMap(rd.gameData.home).id)) {
              model.Results.where(_.gameId === g.id).delete
              model.Results.autoInc.insert(g.id, rd.)
            }

            model.Games
          }
        }
      }
      if (req.doResultUpdates) {
        deletes.foreach {
          rd => {
          }
        }
      }

      if (req.doResultDeletes) {
        deletes.foreach {
          rd => {
          }
        }
      }

      res.copy( resultsInserted = inserts.toList, resultsUpdated = updates.toList, resultsDeleted = deletes.toList)

    }
  }
