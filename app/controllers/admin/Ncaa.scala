package controllers.admin

import play.api.mvc.{Controller, Action}
import models.Repository
import scraping.GameUpdateRequest
import play.api.data.Form
import play.api.data.Forms._
import scraping.GameUpdateRequest
import scala.Some

object Ncaa extends Controller {

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



  def index = Action {
    Ok(views.html.ncaaIndex(gameForm.fill(GameUpdateRequest())))
  }

  def scrapeTeams = Action {
    val data: List[(String, models.Team)] = scraping.PlayTeamScraper.teamRawData()
    play.api.db.slick.DB.withSession {
      try {
        repo.scrapeNcaaTeamsAndConferences(data)
      }
      catch {
        case e: Exception => Redirect(routes.Database.index()).flashing("error" -> "Problem rebuilding the database")
      }
      //TODO Gaaaahhhh terrible!
      Redirect("/app/database").flashing("success" -> "Scraped!")
    }
  }

  def scrapeGames = TODO
  def scrapeGamesEtc = TODO


}
