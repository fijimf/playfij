package controllers.admin

import play.api.mvc.Controller
import play.api.data.Form
import play.api.data.Forms._
import scala.Some
import models.Repository

object KenPom extends Controller {

  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  val kenPomLoadForm: Form[(String, Boolean)] = Form(
    mapping(
      "url" -> nonEmptyText,
      "writeToDb" -> boolean
    )((url, writeToDb) => (url, writeToDb))
      (tup => Some(tup))
  )

  def scrapeGames = TODO

  def index = TODO



}
