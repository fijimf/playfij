package controllers.admin

import scala.concurrent.duration._
import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import models._
import org.joda.time.LocalDate
import play.api.Logger
import scraping.PlayGameScraper
import scala.Some
import scala.concurrent.{Await, Future}


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


case class KenPomUpdateResult(badFormat: List[String] = List.empty[String],
                              unknownTeam: List[String] = List.empty[String],
                              noSeason: List[String] = List.empty[String],
                              outsideRange: List[String] = List.empty[String],
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
        implicit s=>
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

object KenPomScraper {

  import play.api.Play.current

  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  import model.profile.simple._

  private val teamDao: TeamDao = TeamDao(model)
  private val seasonDao = new ConferenceAssociationDao(model)
  private val gameDao = new ConferenceAssociationDao(model)
  private val resultDao = new ConferenceAssociationDao(model)

  val seasons = (for (s <- model.Seasons) yield s).list

  def dateOk(d:LocalDate):Boolean = {
    seasons.foldLeft(false)((b: Boolean, season: Season) => b || inOptRange(d, Some(season.from), Some(season.to))) &&
      inOptRange(d,req.fromDate,req.toDate)

  }

  def inOptRange(dt: LocalDate, from: Option[LocalDate], to: Option[LocalDate]): Boolean = {
    from.map(f => f.isEqual(dt) || f.isBefore(dt)).getOrElse(true) &&
      to.map(t => t.isEqual(dt) || t.isAfter(dt)).getOrElse(true)
  }

  def scrape(repo: Repository, req: KenPomUpdateRequest)(implicit s: scala.slick.session.Session): KenPomUpdateResult = {
    val teamsWithAliases: Map[Team, List[String]] = teamDao.listWithAliases
    val teamMap = teamsWithAliases.keys.map(t => t.name -> t).toMap
    val aliasMap = teamsWithAliases.keys.foldLeft(Map.empty[String, Team])((map: Map[String, Team], team: Team) => teamsWithAliases(team).foldLeft(map)((m2: Map[String, Team], alias: String) => m2 + (alias -> team)))
    val dbData: List[(LocalDate, String, String, Option[(Int, Int)])] = (for (
      (g, r) <- model.Games leftJoin model.Results on (_.id === _.gameId);
      ht <- model.Teams if g.homeTeamId === ht.id;
      at <- model.Teams if g.awayTeamId === at.id
    ) yield (g, r, ht, at)).list().map{case (game: Game, result: Result, home: Team, away: Team) =>{
      (game.date, home.key, away.key, Option(result).map(r=>(r.homeScore, r.awayScore)))
    } }.filter(tup=>dateOk(tup._1))

    val kpData: List[(LocalDate, String, String, Option[(Int, Int)])] = Await.result(PlayGameScraper.scrapeKenPom(req.url),2.minutes).filter(tup=>dateOk(tup._1))

    handleGames(req, dbData, kpData)
    handleResults(req, dbData, kpData)
  }

  def handleGames(req:KenPomUpdateRequest, res:KenPomUpdateResult = KenPomUpdateResult()):KenPomUpdateResult = res
  def handleResults(req:KenPomUpdateRequest, res:KenPomUpdateResult = KenPomUpdateResult()):KenPomUpdateResult = res


}
