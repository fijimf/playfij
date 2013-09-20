package models

import play.api.db.slick.Profile
import org.joda.time.LocalDate
import models.util.LocalDateMapper._
import scala.slick.driver.ExtendedProfile

case class Season(id: Long, key:String, season: String, from:LocalDate, to:LocalDate)

trait SeasonDao {
  this: Profile =>

  import profile.simple._

  object Seasons extends Table[Season]("seasons") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def key = column[String]("season_key")
    def season = column[String]("season")
    def from = column[LocalDate]("from")
    def to = column[LocalDate]("to")

    def * = id ~ key ~ season ~ from ~ to <>(Season.apply _, Season.unapply _)

    def autoInc = key ~ season ~ from ~ to returning id

    def keyIndex = index("sea_key", key, unique = true)
    def seasonIndex = index("sea_seas", season, unique = true)
  }


  def list(implicit s:scala.slick.session.Session): List[Season] = {
    Query(Seasons).sortBy(_.key).to[List]
  }

  def find(key: String)(implicit s:scala.slick.session.Session): Option[Season] = {
    Query(Seasons).where(_.key === key).firstOption
  }

  def update(season: Season)(implicit s:scala.slick.session.Session) {
    Seasons.where(_.id === season.id).update(season)
  }

  def insert(season: Season)(implicit s:scala.slick.session.Session) {
    Seasons.autoInc.insert(season.key, season.season, season.from, season.to)
  }

  def delete(id: String)(implicit s:scala.slick.session.Session) {
    Seasons.where(_.id === id.toLong).delete
  }


  object SeasonDao {
    def apply(p:ExtendedProfile): SeasonDao = {
      new SeasonDao with Profile {
        val profile = p
      }
    }
  }
}