package controllers.admin

import scala.concurrent.duration._
import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import models._
import org.joda.time.{ReadablePartial, LocalDate}
import play.api.Logger
import scraping.{KenPomGameScraper}
import scala.Some
import scala.concurrent.{Await, Future}
import scraping.control.GameUpdateRequest
import securesocial.core.SecureSocial


object KenPom extends Controller with SecureSocial  {
  val logger = Logger("KenPom")

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val form: Form[GameUpdateRequest] = Form(
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
      Ok(views.html.kenpomScrape(form.fill(GameUpdateRequest())))
  }

  def scrapeGames = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          form.bindFromRequest.fold(
            errors => {
              logger.info("Problems saving " + errors)
              BadRequest(views.html.kenpomScrape(errors))
            },
            req => {
              val result = KenPomGameScraper.scrape(repo, req)
              Ok(views.html.kenpomScrapeResult(result, req, TeamDao(repo.m).list))
            }
          )
      }
  }
}

