package models

case class Season(id: Long, year: String)

trait SeasonDao {
  this: Profile =>


  object Seasons extends Table[Season]("seasons") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def year = column[String]("year")

    def * = id ~ year <>(Season.apply _, Season.unapply _)

    def autoInc = year returning id

    def yearIndex = index("sea_year", year, unique = true)
  }

}