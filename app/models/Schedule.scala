package models

import org.joda.time.DateMidnight

case class Schedule(season:Season, conferences:Map[String, (Conference, List[Team])], teams:Map[String,(Team, Conference, List[Game])], dates:Map[DateMidnight, List[Game]], results:Map[Game, Result])
