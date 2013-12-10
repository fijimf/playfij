package controllers.admin

import play.api.mvc.{Controller, Action}
import models.Repository
import securesocial.core.SecureSocial
import play.api.cache.{EhCachePlugin, Cache}

object Admin extends Controller with SecureSocial {
  import play.api.Play.current

  private val repo: Repository = new Repository(play.api.db.slick.DB.driver)

  def index = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        Ok(views.html.adminIndex(repo.checkDatabase()))
      }
  }

  def invalidateCache = SecuredAction {
    implicit request =>
      play.api.db.slick.DB.withSession {
        for(p <- current.plugin[EhCachePlugin]){
          p.manager.clearAll()
        }
        Ok(views.html.adminIndex(repo.checkDatabase()))
      }
  }
}
