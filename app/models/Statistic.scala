package models

import org.joda.time.{DateTime, LocalDate}
import play.api.Logger
import org.saddle.Series
import play.api.cache.Cache
import java.util.Date

case class Statistic(
                      id: Long,
                      key: String,
                      name: String,
                      modelId: Long,
                      targetDomain: String,
                      shortFormat: String,
                      longFormat: String,
                      higherIsBetter: Boolean,
                      displayOrder:Int,
                      color:Option[String],
                      description:Option[String]
                      )


case class StatisticDao(model: Model) {

  import model._
  import model.profile.simple._
  import models.util.Mappers._
  import play.api.Play.current


  val logger = Logger("StatisticDao")
  def list(implicit s: scala.slick.session.Session): List[Statistic] = {
    Query(Statistics).sortBy(_.id).to[List]
  }

  def find(id: Long)(implicit s: scala.slick.session.Session): Option[Statistic] = {
    Query(Statistics).where(_.id === id).firstOption
  }

  def findByKey(key:String)(implicit s: scala.slick.session.Session): Option[Statistic] = {
    Query(Statistics).where(_.key === key).firstOption
  }

  def update(quote: Statistic)(implicit s: scala.slick.session.Session) {
    Statistics.where(_.id === quote.id).update(quote)
  }

  def insert(stat: Statistic)(implicit s: scala.slick.session.Session) {
    Statistics.autoInc.insert(stat.key, stat.name, stat.modelId, stat.targetDomain, stat.shortFormat, stat.longFormat, stat.higherIsBetter, stat.displayOrder, stat.color, stat.description)
  }

  def delete(id: Long)(implicit s: scala.slick.session.Session) {
    Statistics.where(_.id === id).delete
  }

    def loadStats(date: LocalDate,sm:Map[Long, Statistic], tm:Map[Long, Team])(implicit s: scala.slick.session.Session): List[(Statistic, Series[Team, Double])] = {
      Cache.getOrElse[List[(Statistic, Series[Team, Double])]]("stats:" + date.toString("yyyy-MM-dd"), 3600) {
        logger.info("Starting loadStats: (Cache miss on stats):" + date.toString("yyyy-MM-dd") )
        val statDate: LocalDate = getStatDates(date)
        val os = (for {obs <- model.Observations if obs.date === statDate} yield obs).list
        val stats = os.groupBy(o => sm(o.statisticId)).mapValues(lst => Series(lst.map(o => (tm(o.domainId), o.value)): _*)).toList
        logger.info("Finished loadStats")
        stats
      }
    }

    def getStatDates(d: LocalDate)(implicit s: scala.slick.session.Session): LocalDate = {
        Query(model.Observations.map(_.date).max).first().getOrElse(new LocalDate())
    }

    val q = for (stat <- model.Statistics) yield stat
}