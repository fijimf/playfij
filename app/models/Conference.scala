package models

import scala.slick.lifted.DDL
import org.apache.commons.lang3.StringUtils

case class Conference(id: Long,
                      key:String,
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

    override def ddl: DDL = {
      var constraints: DDL = DDL(
        Nil,
        List(
          "ALTER TABLE \"conferences\" ADD CONSTRAINT \"checkKey\" CHECK (\"conference_key\"<>'')",
          "ALTER TABLE \"conferences\" ADD CONSTRAINT \"checkName\" CHECK (\"name\"<>'')",
          "ALTER TABLE \"conferences\" ADD CONSTRAINT \"checkShortName\" CHECK (\"short_name\"<>'')",
          "ALTER TABLE \"conferences\" ADD CONSTRAINT \"checkUrl\" CHECK (\"official_url\"<>'')",
          "ALTER TABLE \"conferences\" ADD CONSTRAINT \"checkTwitter\" CHECK (\"official_twitter\"<>'')",
          "ALTER TABLE \"conferences\" ADD CONSTRAINT \"checkLogo\" CHECK (\"logo_url\"<>'')"
        ),
        List(
          "DROP CONSTRAINT \"checkKey\"",
          "DROP CONSTRAINT \"checkName\"",
          "DROP CONSTRAINT \"checkShortName\"",
          "DROP CONSTRAINT \"checkUrl\"",
          "DROP CONSTRAINT \"checkTwitter\"",
          "DROP CONSTRAINT \"checkLogo\""
        ),
        Nil)
      super.ddl ++ constraints
    }

  }


}