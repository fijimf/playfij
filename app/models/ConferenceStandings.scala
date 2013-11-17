package models

case class ConferenceStandings(conference:Conference, records:List[(Team, (Record, Record))]) {
  def getTeam(key:String):Option[(Team, (Record, Record))] = records.find(_._1.key == key)
}
