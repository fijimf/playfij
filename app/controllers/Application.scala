package controllers

import play.api.mvc.{Controller, Action}
import models.{DatabaseStatus, Repository}
import securesocial.core.SecureSocial
import org.joda.time.DateTime

object Application extends Controller with SecureSocial  {



  def about = UserAwareAction {
    implicit request =>

    Ok(views.html.about("Your new application is ready."))
  }

}