package models

import org.apache.commons.lang3.StringUtils

case class Conference(id: Long,
                      key: String,
                      name: String,
                      shortName: String,
                      officialUrl: Option[String],
                      officialTwitter: Option[String],
                      logoUrl: Option[String]) {
  require(StringUtils.isNotBlank(key), "Name cannot be blank")
  require(StringUtils.isNotBlank(name), "Name cannot be blank")
  require(StringUtils.isNotBlank(shortName), "Short name cannot be blank")
  require(officialUrl.map(StringUtils.isNotBlank).getOrElse(true), "Official URL cannot be blank")
  require(officialTwitter.map(StringUtils.isNotBlank).getOrElse(true), "Official twitter cannot be blank")
  require(logoUrl.map(StringUtils.isNotBlank).getOrElse(true), "logo URL cannot be blank")

  val logoUrlSmall = logoUrl.map(_.replaceAll("""70\.png$""", "40.png"))
}

case class ConferenceDao(model: Model) {

  import model._
  import model.profile.simple._


  def list(implicit s: scala.slick.session.Session): List[Conference] = {
    Query(Conferences).sortBy(_.name).to[List]
  }

  def find(key: String)(implicit s: scala.slick.session.Session): Option[Conference] = {
    Query(Conferences).where(_.key === key).firstOption
  }

  def update(conference: Conference)(implicit s: scala.slick.session.Session) {
    Conferences.where(_.id === conference.id).update(conference)
  }

  def insert(conference: Conference)(implicit s: scala.slick.session.Session) {
    Conferences.autoInc.insert(conference.key, conference.name, conference.shortName, conference.logoUrl, conference.officialUrl, conference.officialTwitter)
  }

  def delete(id: String)(implicit s: scala.slick.session.Session) {
    Conferences.where(_.id === id.toLong).delete
  }

  val assocQ = for (assoc <- model.ConferenceAssociations) yield assoc
  val confQ = for (conf <- model.Conferences) yield conf

}
