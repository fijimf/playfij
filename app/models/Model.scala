package models

import play.api.db.slick.Profile
import org.joda.time.LocalDate
import org.joda.time.LocalDate
import models.util.LocalDateMapper._

trait Model extends Profile{

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

  object Conferences extends Table[Conference]("conferences") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def key = column[String]("conference_key")

    def name = column[String]("name")

    def shortName = column[String]("short_name")

    def officialUrl = column[Option[String]]("official_url")

    def officialTwitter = column[Option[String]]("official_twitter")

    def logoUrl = column[Option[String]]("logo_url")

    def * = id ~ key ~ name ~ shortName ~ officialUrl ~ officialTwitter ~ logoUrl <>(Conference.apply _, Conference.unapply _)

    def autoInc = key ~ name ~ shortName ~ officialUrl ~ officialTwitter ~ logoUrl returning id

    def keyIndex = index("cnf_key", key, unique = true)

    def nameIndex = index("cnf_name", name, unique = true)

    def shortNameIndex = index("conf_short_name", shortName, unique = true)

  }

  object Teams extends Table[Team]("teams") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def key = column[String]("team_key")

    def name = column[String]("name")

    def longName = column[String]("long_name")

    def nickname = column[String]("nickname")

    def primaryColor = column[Option[String]]("primary_color")

    def secondaryColor = column[Option[String]]("secondary_color")

    def logoUrl = column[Option[String]]("logo_url")

    def officialUrl = column[Option[String]]("official_url")

    def officialTwitter = column[Option[String]]("official_twitter")

    def * = id ~ key ~ name ~ longName ~ nickname ~ primaryColor ~ secondaryColor ~ logoUrl ~ officialUrl ~ officialTwitter <>(Team.apply _, Team.unapply _)

    def autoInc = key ~ name ~ longName ~ nickname ~ primaryColor ~ secondaryColor ~ logoUrl ~ officialUrl ~ officialTwitter returning id

    def keyIndex = index("tea_key", key, unique = true)

    def nameIndex = index("tea_name", name, unique = true)

    def longNameIndex = index("tea_long_name", longName, unique = true)
  }

  object Aliases extends Table[Alias]("aliases") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def teamId = column[Long]("team_id")

    def alias = column[String]("alias")

    def * = id ~ teamId ~ alias <>(Alias.apply _, Alias.unapply _)

    def autoInc = teamId ~ alias returning id

    def teamFk = foreignKey("als_team_fk", teamId, Teams)(_.id)

    def aliasIndex = index("als_alias", alias, unique = true)
  }

  object ConferenceAssociations extends Table[ConferenceAssociation]("conference_associations") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def seasonId = column[Long]("season_id")

    def conferenceId = column[Long]("conference_id")

    def teamId = column[Long]("team_id")

    def * = id ~ seasonId ~ conferenceId ~ teamId <>(ConferenceAssociation.apply _, ConferenceAssociation.unapply _)

    def autoInc = seasonId ~ conferenceId ~ teamId returning id

    def seasonFk = foreignKey("cas_season_fk", seasonId, Seasons)(_.id)

    def teamFk = foreignKey("cas_team_fk", teamId, Teams)(_.id)

    def conferenceFk = foreignKey("cas_conference_fk", conferenceId, Conferences)(_.id)

    def teamIndex = index("cas_season_team", (seasonId, teamId), unique = true)
  }

  object Games extends Table[Game]("games") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def seasonId = column[Long]("season_id")

    def homeTeamId = column[Long]("home_team_id")

    def awayTeamId = column[Long]("away_team_id")

    def date = column[LocalDate]("date")

    def resultId = column[Long]("result_id")

    def location = column[Option[String]]("location")

    def isNeutralSite = column[Boolean]("is_neutral_site")

    def * = id ~ seasonId ~ homeTeamId ~ awayTeamId ~ date ~ location ~ isNeutralSite <>(Game.apply _, Game.unapply _)

    def autoInc = seasonId ~ homeTeamId ~ awayTeamId ~ date ~ location ~ isNeutralSite returning id

    def homeTeamFk = foreignKey("gam_home_team_fk", homeTeamId, Teams)(_.id)

    def awayTeamFk = foreignKey("gam_away_team_fk", homeTeamId, Teams)(_.id)

    def seasonFk = foreignKey("gam_season_fk", seasonId, Seasons)(_.id)
  }

  object Results extends Table[Result]("results") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def gameId = column[Long]("season_id")

    def homeScore = column[Int]("home_score")

    def awayScore = column[Int]("away_score")

    def numOts = column[Int]("num_ots")

    def * = id ~ gameId ~ homeScore ~ awayScore ~ numOts <>(Result.apply _, Result.unapply _)

    def autoInc = homeScore ~ awayScore ~ numOts returning id

    def gameFk = foreignKey("res_game_fk", gameId, Games)(_.id)

    def indexGame= index("res_game", gameId, unique = true)

  }

  object Quotes extends Table[Quote]("quotes") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def quote = column[String]("quote")

    def source = column[Option[String]]("source")

    def url = column[Option[String]]("url")

    def * = id ~ quote ~ source ~ url <>(Quote.apply _, Quote.unapply _)

    def autoInc =  quote ~ source ~ url returning id

  }

  object Users extends Table[User]("users") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def name = column[String]("name")

    def password = column[String]("password")

    def email = column[String]("email")

    def * = id ~ name ~ password ~ email <>(User.apply _, User.unapply _)

    def autoInc = id ~ name ~ password ~ email <>(User.apply _, User.unapply _)

    def nameIndex = index("usr_name", name, unique = true)

    def emailIndex = index("usr_email", email, unique = true)
  }

}
