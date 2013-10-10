package controllers.admin

import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import models.Repository
import org.joda.time.LocalDate
import play.api.Logger

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

object KenPomScraper {
  def scrape(repo: Repository, req: KenPomUpdateRequest): KenPomUpdateResult = {
    KenPomUpdateResult()


  }


}
