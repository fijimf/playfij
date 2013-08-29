package models

import org.joda.time.DateMidnight
import util.DateMidnightMapper._
import play.api.db.slick.Profile

case class Game(id: Long, seasonId: Long, homeTeamId: Long, awayTeamId: Long, date: DateMidnight, location: Option[String], isNeutralSite: Boolean) {
  require(homeTeamId != awayTeamId)
}


trait GameDao {
  self: Profile with TeamDao with SeasonDao =>

  import profile.simple._

  object Games extends Table[Game]("games") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def seasonId = column[Long]("season_id")

    def homeTeamId = column[Long]("home_team_id")

    def awayTeamId = column[Long]("away_team_id")

    def date = column[DateMidnight]("date")

    def resultId = column[Long]("result_id")

    def location = column[Option[String]]("location")

    def isNeutralSite = column[Boolean]("is_neutral_site")

    def * = id ~ seasonId ~ homeTeamId ~ awayTeamId ~ date ~ location ~ isNeutralSite <>(Game.apply _, Game.unapply _)

    def autoInc = seasonId ~ homeTeamId ~ awayTeamId ~ date ~ location ~ isNeutralSite returning id

    def homeTeamFk = foreignKey("gam_home_team_fk", homeTeamId, Teams)(_.id)

    def awayTeamFk = foreignKey("gam_away_team_fk", homeTeamId, Teams)(_.id)

    def seasonFk = foreignKey("gam_season_fk", seasonId, Seasons)(_.id)
  }

}