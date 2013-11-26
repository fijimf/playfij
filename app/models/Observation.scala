package models

import org.joda.time.LocalDate

case class Observation(
                        id: Long,
                        date: LocalDate,
                        domainId: Long,
                        statisticId: Long,
                        value: Double
                        )

case class ObservationDao(model: Model) {

  import model._
  import model.profile.simple._

  def list(implicit s: scala.slick.session.Session): List[Observation] = {
    Query(Observations).sortBy(_.id).to[List]
  }

  def find(id: Long)(implicit s: scala.slick.session.Session): Option[Observation] = {
    Query(Observations).where(_.id === id).firstOption
  }


  def update(obs: Observation)(implicit s: scala.slick.session.Session) {
    Observations.where(_.id === obs.id).update(obs)
  }

  def insert(obs: Observation)(implicit s: scala.slick.session.Session) {
    Observations.autoInc.insert(obs.date, obs.domainId, obs.statisticId, obs.value)
  }

  def delete(id: Long)(implicit s: scala.slick.session.Session) {
    Observations.where(_.id === id).delete
  }


}