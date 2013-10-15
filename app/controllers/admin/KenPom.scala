package controllers.admin

import scala.concurrent.duration._
import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import models._
import org.joda.time.{ReadablePartial, LocalDate}
import play.api.Logger
import scraping.PlayGameScraper
import scala.Some
import scala.concurrent.{Await, Future}
import java.io.Serializable


case class KenPomUpdateRequest(url: String = "",
                               doWrite: Boolean = false,
                               doGameInserts: Boolean = true,
                               doGameUpdates: Boolean = true,
                               doGameDeletes: Boolean = false,
                               doResultInserts: Boolean = true,
                               doResultUpdates: Boolean = true,
                               doResultDeletes: Boolean = false,
                               fromDate: Option[LocalDate] = Some(new LocalDate()),
                               toDate: Option[LocalDate] = None)


case class KenPomUpdateResult(
                               unknownTeam: List[String] = List.empty[String],
                               dateOutsideRange: List[String] = List.empty[String],
                               gamesInserted: List[(LocalDate, String, String)] = List.empty[(LocalDate, String, String)],
                               gamesUpdated: List[(LocalDate, String, String)] = List.empty[(LocalDate, String, String)],
                               gamesDeleted: List[(LocalDate, String, String)] = List.empty[(LocalDate, String, String)],
                               resultsInserted: List[(LocalDate, String, String, Int, Int)] = List.empty[(LocalDate, String, String, Int, Int)],
                               resultsUpdated: List[(LocalDate, String, String, Int, Int)] = List.empty[(LocalDate, String, String, Int, Int)],
                               resultsDeleted: List[(LocalDate, String, String, Int, Int)] = List.empty[(LocalDate, String, String, Int, Int)])

object KenPom extends Controller {
  val logger = Logger("KenPom")

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val form: Form[KenPomUpdateRequest] = Form(
    mapping(
      "url" -> nonEmptyText,
      "doWrite" -> boolean,
      "doGameInserts" -> boolean,
      "doGameUpdates" -> boolean,
      "doGameDeletes" -> boolean,
      "doResultInserts" -> boolean,
      "doResultUpdates" -> boolean,
      "doResultDeletes" -> boolean,
      "fromDate" -> optional(jodaLocalDate),
      "toDate" -> optional(jodaLocalDate)
    )((url, doWrite, doGameInserts, doGameUpdates, doGameDeletes, doResultInserts, doResultUpdates, doResultDeletes, fromDate, toDate) =>
      KenPomUpdateRequest(url, doWrite, doGameInserts, doGameUpdates, doGameDeletes, doResultInserts, doResultUpdates, doResultDeletes, fromDate, toDate))
      (k => Some((k.url, k.doWrite, k.doGameInserts, k.doGameUpdates, k.doGameDeletes, k.doResultInserts, k.doResultUpdates, k.doResultDeletes, k.fromDate, k.toDate))
      )
  )


  def index = Action {
    implicit request =>
      Ok(views.html.kenpomScrape(form.fill(KenPomUpdateRequest()), None))
  }

  def scrapeGames = Action {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          form.bindFromRequest.fold(
            errors => {
              logger.info("Problems saving " + errors)
              BadRequest(views.html.kenpomScrape(errors, None))
            },
            req => {
              val result = KenPomScraper.scrape(repo, req)
              Ok(views.html.kenpomScrape(form.fill(KenPomUpdateRequest()), Some(result)))
            }
          )
      }
  }
}

//TODO Refactor this abomination!!!!!!!!!!!!

object KenPomScraper {

  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  import model.profile.simple._

  private val teamDao: TeamDao = TeamDao(model)

  def seasons(implicit s: scala.slick.session.Session) = (for (s <- model.Seasons) yield s).list

  def dateOk(d: LocalDate, req: KenPomUpdateRequest)(implicit s: scala.slick.session.Session): Boolean = {
    seasons.foldLeft(false)((b: Boolean, season: Season) => b || inOptRange(d, Some(season.from), Some(season.to))) &&
      inOptRange(d, req.fromDate, req.toDate)

  }

  def inOptRange(dt: LocalDate, from: Option[LocalDate], to: Option[LocalDate]): Boolean = {
    from.map(f => f.isEqual(dt) || f.isBefore(dt)).getOrElse(true) &&
      to.map(t => t.isEqual(dt) || t.isAfter(dt)).getOrElse(true)
  }

  def scrape(repo: Repository, req: KenPomUpdateRequest)(implicit s: scala.slick.session.Session): KenPomUpdateResult = {
    val dbData: List[(LocalDate, String, String, Option[(Int, Int)])] = (for (
      (g, r) <- model.Games leftJoin model.Results on (_.id === _.gameId);
      ht <- model.Teams if g.homeTeamId === ht.id;
      at <- model.Teams if g.awayTeamId === at.id
    ) yield (g, r, ht, at)).list().map {
      case (game: Game, result: Result, home: Team, away: Team) => {
        (game.date, home.key, away.key, Option(result).map(r => (r.homeScore, r.awayScore)))
      }
    }.filter(tup => dateOk(tup._1, req))

    val (kpData, badDate) = Await.result(PlayGameScraper.scrapeKenPom(req.url), 2.minutes).partition(tup => dateOk(tup._1, req))
    val r1 = KenPomUpdateResult(dateOutsideRange = badDate.map(_._1.toString))
      val hg = handleGames(req, dbData, kpData, _:KenPomUpdateResult)
      val hr = handleResults(req, dbData, kpData, _:KenPomUpdateResult)
      hg.andThen(hr).apply(r1)
  }

  def handleGames(req: KenPomUpdateRequest, dbData: List[(LocalDate, String, String, Option[(Int, Int)])], kpDate: List[(LocalDate, String, String, Option[(Int, Int)])], res: KenPomUpdateResult = KenPomUpdateResult()): KenPomUpdateResult = {
    val teamsWithAliases: Map[Team, List[String]] = teamDao.listWithAliases
    val teamKeyMap: Map[String, Team] = teamsWithAliases.keys.map(t=>t.key->t).toMap
    val teamMap = teamsWithAliases.keys.map(t => t.name.toLowerCase -> t).toMap
    val aliasMap = teamsWithAliases.keys.foldLeft(Map.empty[String, Team])((map: Map[String, Team], team: Team) => teamsWithAliases(team).foldLeft(map)((m2: Map[String, Team], alias: String) => m2 + (alias.toLowerCase -> team)))

    def lookup(t:String):Option[Team] = teamMap.get(t).orElse(aliasMap.get(t))

    val z: (List[(LocalDate, String, String)], List[(LocalDate, String, String)]) = (List.empty[(LocalDate, String, String)], List.empty[(LocalDate, String, String)])
    val (good, bad) = kpDate.foldLeft(z)((goodBad: (List[(LocalDate, String, String)], List[(LocalDate, String, String)]), g: (LocalDate, String, String, Option[(Int, Int)])) => {
        (lookup(g._2), lookup(g._3)) match {
          case (Some(h), Some(a)) => ((g._1, h.key, a.key) :: goodBad._1, goodBad._2)
          case _ => (goodBad._1, (g._1,g._2,g._3) :: goodBad._2)
        }
      })

    val kpSet: Set[(LocalDate, String, String)] = good.toSet
    val dbSet = dbData.map(t=>(t._1, t._2, t._3)).toSet

    val inserts = kpSet.diff(dbSet).toList
    val deletes = dbSet.diff(kpSet).toList
    val updates = List.empty
    if (req.doGameInserts) {
      inserts.foreach {
        case (date: LocalDate, homeKey: String, awayKey: String) => {
          val season: Option[Season] = seasons.find(s => inOptRange(date, Some(s.from), Some(s.to)))
          model.Games.autoInc.insert(season.get.id, teamKeyMap(homeKey).id, teamKeyMap(awayKey).id, date, None, false)
        }
      }
    }
    if (req.doGameDeletes) {
      deletes.foreach {
        case (date: LocalDate, homeKey: String, awayKey: String) => {
          val season: Option[Season] = seasons.find(s => inOptRange(date, Some(s.from), Some(s.to)))
          model.Games.where(g=>(g.seasonId === season.get.id) && (g.homeTeamId === teamKeyMap(homeKey).id) && (g.awayTeamId === teamKeyMap(awayKey).id)).delete
        }
      }
    }

    res.copy(unknownTeam = bad.map(_.toString()), gamesInserted = inserts, gamesUpdated = updates, gamesDeleted = deletes)

  }

  def handleResults(req: KenPomUpdateRequest, dbData: List[(LocalDate, String, String, Option[(Int, Int)])], kpData: List[(LocalDate, String, String, Option[(Int, Int)])], res: KenPomUpdateResult = KenPomUpdateResult()): KenPomUpdateResult = res


}
