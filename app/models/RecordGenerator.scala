package models

trait RecordGenerator {
  self: RecordGenerator =>
  def label: String

  def filter(data: List[ResultData]): List[ResultData]

  def apply(team: Team, data: List[ResultData]): Record = {
    filter(data).foldLeft(Record())((record: Record, d: ResultData) => {
      if (d.homeTeam.key == team.key && d.result.homeScore > d.result.awayScore) {
        record.addWin()
      } else {
        record.addLoss()
      }
    })
  }

  def +(rg: RecordGenerator): RecordGenerator = {
    new RecordGenerator {
      def filter(data: List[ResultData]): List[ResultData] = rg.filter(self.filter(data))

      def label: String = self.label + rg.label
    }
  }
}


object SeasonRecord {
  def apply(season:Season): RecordGenerator = {
    new RecordGenerator {
      def filter(data: List[ResultData]): List[ResultData] = data.filter(_.season.id==season.id).sortBy(_.game.date.toDate())
      def label: String = season.key
    }
  }
}

object ConferenceRecord extends RecordGenerator {
  def label: String = "Conference"

  def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> d.homeConference.key == d.awayConference.key)
}

object NonConferenceRecord extends RecordGenerator {
  def label: String = "NonConference"

  def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> d.homeConference.key != d.awayConference.key)
}
object HomeRecord {
  def apply(t:Team) = new RecordGenerator {
    def label: String = "Home"

    def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> !d.game.isNeutralSite && d.homeTeam.key == t.key)
  }
}
object AwayRecord {
  def apply(t:Team) = new RecordGenerator {
    def label: String = "Away"

    def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> !d.game.isNeutralSite && d.awayTeam.key == t.key)
  }
}
object NeutralRecord extends RecordGenerator {
  def label: String = "Conference"

  def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> d.game.isNeutralSite)
}

object LastNRecord {
  def apply(n: Int): RecordGenerator = {
    new RecordGenerator {
      def filter(data: List[ResultData]): List[ResultData] = data.sortBy(_.game.date.toDate()).reverse.take(n).reverse

      def label: String = "Last %d".format(n)
    }
  }
}

object MonthRecord {
  def apply(month: Int): RecordGenerator = {
    new RecordGenerator {
      def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> d.game.date.getMonthOfYear==month)

      def label: String = "Month %d".format(month)
    }
  }
}

object LessThanMarginRecord {
  def apply(margin: Int): RecordGenerator = {
    new RecordGenerator {
      def filter(data: List[ResultData]): List[ResultData] = data.filter(d=> math.abs(d.result.homeScore - d.result.awayScore)<margin)

      def label: String = "< %d".format(margin)
    }
  }
}
