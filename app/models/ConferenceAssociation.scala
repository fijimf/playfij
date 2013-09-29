package models

import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class ConferenceAssociation(id: Long,
                                 seasonId: Long,
                                 conferenceId: Long,
                                 teamId: Long)

trait ConferenceAssociationDao {

  self: Profile  =>
  val conferenceDao: ConferenceDao = models.ConferenceDao(profile)
  val seasonDao: SeasonDao = models.SeasonDao(profile)
  val teamDao: TeamDao = models.TeamDao(profile)
  import profile.simple._

  object ConferenceAssociations extends Table[ConferenceAssociation]("conference_associations") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def seasonId = column[Long]("season_id")

    def conferenceId = column[Long]("conference_id")

    def teamId = column[Long]("team_id")

    def * = id ~ seasonId ~ conferenceId ~ teamId <>(ConferenceAssociation.apply _, ConferenceAssociation.unapply _)

    def autoInc = seasonId ~ conferenceId ~ teamId returning id

    def seasonFk = foreignKey("cas_season_fk", seasonId, seasonDao.Seasons)(_.id)

    def teamFk = foreignKey("cas_team_fk", teamId, teamDao.Teams)(_.id)

    def conferenceFk = foreignKey("cas_conference_fk", conferenceId, conferenceDao.Conferences)(_.id)

    def teamIndex = index("cas_season_team", (seasonId, teamId), unique = true)
  }

  def queryByTeam(t: Team)(implicit s: scala.slick.session.Session) : Map[Season, Conference] = {
    (for (a <- ConferenceAssociations if (a.teamId === t.id);
          s <- seasonDao.Seasons if (a.seasonId === s.id);
          c <- conferenceDao.Conferences if (a.conferenceId === c.id)) yield (s -> c)).list().toMap
  }


}


object ConferenceAssociationDao {
  def apply(p: ExtendedProfile): ConferenceAssociationDao = {
    new ConferenceAssociationDao with Profile {
      val profile = p
    }
  }
}