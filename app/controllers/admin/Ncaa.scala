package controllers.admin

import play.api.mvc.Controller
import models.{TeamDao, Repository}
import scraping.NcaaGameScraper
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import play.api.Logger
import scraping.control.GameUpdateRequest
import securesocial.core.SecureSocial

object Ncaa extends Controller with SecureSocial  {
  val logger = Logger("Ncaa")

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val gameForm: Form[GameUpdateRequest] = Form(
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
      GameUpdateRequest(url, doWrite, doGameInserts, doGameUpdates, doGameDeletes, doResultInserts, doResultUpdates, doResultDeletes, fromDate, toDate))
      (k => Some((k.url, k.doWrite, k.doGameInserts, k.doGameUpdates, k.doGameDeletes, k.doResultInserts, k.doResultUpdates, k.doResultDeletes, k.fromDate, k.toDate))
      )
  )



  def index = SecuredAction {
    implicit request =>

    Ok(views.html.ncaaIndex(gameForm.fill(GameUpdateRequest())))
  }

  def scrapeTeams = SecuredAction {
    implicit request =>
    val data: List[(String, models.Team)] = scraping.NcaaTeamScraper.teamRawData()
    play.api.db.slick.DB.withSession {
      try {
        repo.scrapeNcaaTeamsAndConferences(data)
      }
      catch {
        case e: Exception => Redirect(routes.Database.index()).flashing("error" -> "Problem rebuilding the database")
      }
      Redirect(controllers.routes.Team.list).flashing("success" -> "Scraped!")
    }
  }

    def scrapeGames = SecuredAction {
      implicit request =>
        play.api.db.slick.DB.withSession {
          implicit s =>
            gameForm.bindFromRequest.fold(
              errors => {
                logger.info("Problems saving " + errors)
                BadRequest(views.html.ncaaIndex(errors))
              },
              req => {
                val result = NcaaGameScraper.scrape(repo, req)
                Ok(views.html.ncaaScrapeResult(result, req, TeamDao(repo.m).list))
              }
            )
        }

  }
  def scrapeGamesEtc = TODO


}
