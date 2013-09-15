package models

import org.apache.commons.lang3.StringUtils
import play.api.db.slick.Profile
import java.util.Date
import scala.slick.driver.ExtendedProfile

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
}

trait ConferenceDao {

  self: Profile =>

  import profile.simple._

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


  def list(implicit s:scala.slick.session.Session): List[Conference] = {
    Query(Conferences).sortBy(_.name).to[List]
  }

  def find(key: String)(implicit s:scala.slick.session.Session): Option[Conference] = {
    Query(Conferences).where(_.key === key).firstOption
  }

  def update(conference: Conference)(implicit s:scala.slick.session.Session) {
    Conferences.where(_.id === conference.id).update(conference)
  }

  def insert(conference: Conference)(implicit s:scala.slick.session.Session) {
    Conferences.autoInc.insert(conference.key, conference.name, conference.shortName, conference.logoUrl, conference.officialUrl, conference.officialTwitter)
  }

  def delete(id: String)(implicit s:scala.slick.session.Session) {
    Conferences.where(_.id === id.toLong).delete
  }

}

object ConferenceDao {
  def apply(p:ExtendedProfile): ConferenceDao = {
    new ConferenceDao with Profile {
      val profile = p
    }
  }
}