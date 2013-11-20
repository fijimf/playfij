package models

/**
 * Created with IntelliJ IDEA.
 * User: Jim
 * Date: 11/19/13
 * Time: 10:34 PM
 * To change this template use File | Settings | File Templates.
 */
case class ResultData (
  season:Season,
  game:Game,
  homeTeam: Team,
  awayTeam: Team,
  homeConference: Conference,
  awayConference: Conference,
  result: Result)
