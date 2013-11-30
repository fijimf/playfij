package models

case class Statistic(
                      id: Long,
                      key: String,
                      name: String,
                      modelId: Long,
                      targetDomain: String,
                      shortFormat: String,
                      longFormat: String,
                      higherIsBetter: Boolean,
                      displayOrder:Int
                      )


case class StatisticDao(model: Model) {

  import model._
  import model.profile.simple._

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
    Statistics.autoInc.insert(stat.key, stat.name, stat.modelId, stat.targetDomain, stat.shortFormat, stat.longFormat, stat.higherIsBetter, stat.displayOrder)
  }

  def delete(id: Long)(implicit s: scala.slick.session.Session) {
    Statistics.where(_.id === id).delete
  }


}