package models


case class StatisticalModel(
                             id: Long,
                             key: String,
                             name: String,
                             className: String
                             )


case class StatisticalModelDao(model: Model) {

  import model._
  import model.profile.simple._

  def list(implicit s: scala.slick.session.Session): List[StatisticalModel] = {
    Query(StatisticalModels).sortBy(_.id).to[List]
  }

  def find(id: Long)(implicit s: scala.slick.session.Session): Option[StatisticalModel] = {
    Query(StatisticalModels).where(_.id === id).firstOption
  }

  def findByKey(key:String)(implicit s: scala.slick.session.Session): Option[StatisticalModel] = {
    Query(StatisticalModels).where(_.key === key).firstOption
  }

  def update(quote: StatisticalModel)(implicit s: scala.slick.session.Session) {
    StatisticalModels.where(_.id === quote.id).update(quote)
  }

  def insert(mod: StatisticalModel)(implicit s: scala.slick.session.Session) {
    StatisticalModels.autoInc.insert(mod.key, mod.name, mod.className)
  }

  def delete(id: Long)(implicit s: scala.slick.session.Session) {
    StatisticalModels.where(_.id === id).delete
  }


}
