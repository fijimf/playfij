package models

import play.api.db.slick.Profile
import org.joda.time.DateMidnight
import models.util.DateMidnightMapper._

case class Season(id: Long, season: String, from:DateMidnight, to:DateMidnight)

trait SeasonDao {
  this: Profile =>

  import profile.simple._

  object Seasons extends Table[Season]("seasons") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def key = column[String]("season_key")
    def season = column[String]("season")
    def from = column[DateMidnight]("from")
    def to = column[DateMidnight]("to")

    def * = id ~ key ~ season ~ from ~ to <>(Season.apply _, Season.unapply _)

    def autoInc = key ~ season ~ from ~ to returning id

    def keyIndex = index("sea_key", key, unique = true)
    def seasonIndex = index("sea_seas", season, unique = true)
  }

}