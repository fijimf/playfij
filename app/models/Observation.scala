package models

import org.joda.time.LocalDate
import util.Mappers._

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

  def insertAll(obss: Iterable[Observation])(implicit s: scala.slick.session.Session) {
    Observations.autoInc.insertAll(obss.toSeq.map(o=>(o.date, o.domainId, o.statisticId, o.value)): _*)
  }

  def delete(id: Long)(implicit s: scala.slick.session.Session) {
    Observations.where(_.id === id).delete
  }
  def deleteByDateStat(statId: Long, date:LocalDate)(implicit s: scala.slick.session.Session) {
    Observations.where(o => o.statisticId === statId && o.date === date).delete
  }

  def deleteByDatesStat(statId: Long, start:LocalDate, end:LocalDate)(implicit s: scala.slick.session.Session) {
    Observations.where(o => o.statisticId === statId && o.date >= start && o.date <= end).delete
  }

  def upsert(obs:Observation) (implicit s: scala.slick.session.Session) {
    val option=(for (o<-Observations if o.date === obs.date && o.statisticId === obs.statisticId && o.domainId === obs.domainId) yield o).firstOption
    option  match {
      case Some(oo)=> update(Observation(oo.id, oo.date, oo.domainId, oo.statisticId, obs.value))
      case None => insert(obs)
    }
  }


}