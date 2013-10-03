package models

import play.api.db.slick.Profile
import org.joda.time.LocalDate
import models.util.LocalDateMapper._
import scala.slick.driver.ExtendedProfile

case class Season(id: Long, key: String, season: String, from: LocalDate, to: LocalDate)

case class SeasonDao(model: Model) {

  import model._
  import model.profile.simple._

  def list(implicit s: scala.slick.session.Session): List[Season] = {
    Query(Seasons).sortBy(_.key).to[List]
  }

  def find(key: String)(implicit s: scala.slick.session.Session): Option[Season] = {
    Query(Seasons).where(_.key === key).firstOption
  }

  def update(season: Season)(implicit s: scala.slick.session.Session) {
    Seasons.where(_.id === season.id).update(season)
  }

  def insert(season: Season)(implicit s: scala.slick.session.Session) {
    Seasons.autoInc.insert(season.key, season.season, season.from, season.to)
  }

  def delete(id: String)(implicit s: scala.slick.session.Session) {
    Seasons.where(_.id === id.toLong).delete
  }
}
