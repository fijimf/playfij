package models

import org.joda.time.{LocalDate, DateMidnight}
import util.Mappers._
import play.api.db.slick.Profile
import scala.slick.driver.ExtendedProfile

case class Game(id: Long, seasonId: Long, homeTeamId: Long, awayTeamId: Long, date: LocalDate, location: Option[String], isNeutralSite: Boolean) {
  require(homeTeamId != awayTeamId)
}

case class GameDao(model: Model) {

  import model._
  import model.profile.simple._

  val game = for {g <- Games} yield g
  val gameWithTeams = for {g <- Games
                           h <- g.homeTeamFk
                           a <- g.awayTeamFk} yield (g, h, a)

  def gameBySchedule(homeTeamKey: String, awayTeamKey: String, date: LocalDate) = {
    gameWithTeams.where(gha => gha._1.date === date && gha._2.key === homeTeamKey && gha._3.key === awayTeamKey)
  }

  def findGameBySchedule(homeTeamKey: String, awayTeamKey: String, date: LocalDate)(implicit s: scala.slick.session.Session):Option[Game] = {
    gameBySchedule(homeTeamKey, awayTeamKey, date).map(_._1).firstOption
  }

  val gameResultQuery = for {(game, result) <- model.Games leftJoin model.Results on (_.id === _.gameId)} yield (game, result.maybe)

  val assocQuery = for (assoc <- model.ConferenceAssociations) yield assoc

  val scheduleQuery = for {
    (game, result) <- gameResultQuery
    season <- game.seasonFk
    homeTeam <- game.homeTeamFk
    awayTeam <- game.awayTeamFk
    homeAssoc <- assocQuery if homeAssoc.teamId === homeTeam.id && homeAssoc.seasonId === season.id
    awayAssoc <- assocQuery if awayAssoc.teamId === awayTeam.id && awayAssoc.seasonId === season.id
    homeConf <- homeAssoc.conferenceFk
    awayConf <- awayAssoc.conferenceFk
  } yield {
    (season, game, homeTeam, awayTeam, homeConf, awayConf, result)
  }

}