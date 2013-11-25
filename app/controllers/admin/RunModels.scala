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


object RunModels extends Controller with SecureSocial  {
  val logger = Logger("RunModels")

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def index = SecuredAction {
    implicit request =>
      Ok("yuk")
  }

}

