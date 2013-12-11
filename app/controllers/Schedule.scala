package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import play.api.Logger
import models._
import models.ScheduleDao

object Schedule extends Controller with SecureSocial {

  import play.api.Play.current

  private val logger = Logger("ScheduleController")
  private val model = new Model() {
    val profile = play.api.db.slick.DB.driver
  }

  private val scheduleDao: ScheduleDao = ScheduleDao(model)
  private val quoteDao = new QuoteDao(model)


  def team(key: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val q = quoteDao.random
          scheduleDao.teamPage(key).map(tp => {
            Ok(views.html.teamView(q, tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }

  def teamSeason(key: String, seasonKey: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val q = quoteDao.random
          scheduleDao.teamPage(key, seasonKey).map(tp => {
            Ok(views.html.teamView(q, tp))
          }).getOrElse(NotFound(views.html.resourceNotFound("team", key)))
      }
  }

  def search(q: String) = UserAwareAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          val quote = quoteDao.random
          Ok(views.html.searchView(quote, scheduleDao.search(q)))
      }
  }

}

