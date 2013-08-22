package models

case class ConferenceAssociation(id: Long,
                                 seasonId: Long,
                                 conferenceId: Long,
                                 teamId: Long)

trait ConferencesAssociationDao {

  self: Profile with SeasonDao with TeamDao with ConferenceDao =>

  import profile.simple._

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


}