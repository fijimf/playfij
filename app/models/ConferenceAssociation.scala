package models

import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class ConferenceAssociation(id: Long,
                                 seasonId: Long,
                                 conferenceId: Long,
                                 teamId: Long)

case class ConferenceAssociationDao (model: Model) {

import model._
import model.profile.simple._

def queryByTeam(t: Team)(implicit s: scala.slick.session.Session) : Map[Season, Conference] = {
    (for (a <- ConferenceAssociations if (a.teamId === t.id);
          s <- Seasons if (a.seasonId === s.id);
          c <- Conferences if (a.conferenceId === c.id)) yield (s -> c)).list().toMap
  }
}


