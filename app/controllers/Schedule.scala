package controllers

import securesocial.core.SecureSocial
import play.mvc.Controller
import models._
import models.ConferenceAssociation
import org.joda.time.LocalDate

object Schedule extends Controller with SecureSocial {
  def team = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        implicit s =>
          Ok(views.html.conferenceList(dao.list))
      }
  }


}

