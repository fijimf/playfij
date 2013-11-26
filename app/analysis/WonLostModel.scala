package analysis

import org.joda.time.LocalDate
import models.{Statistic, ScheduleData}

class WonLostModel extends DerivedModel {
  def key = "won-lost"


  def deriveResults(result: ModelResult): ModelResult = {
    (for (wins <- result.get("wins");
          losses <- result.get("losses"))
    yield {
      val outerKeys: Set[LocalDate] = wins.keySet.intersect(losses.keySet)
      key -> outerKeys.foldLeft(Map.empty[LocalDate, Map[Long, Double]])((outer: Map[LocalDate, Map[Long, Double]], date: LocalDate) => {
        for (w <- wins.get(date);
             l <- losses.get(date)) yield {
          val innerKeys: Set[Long] = w.keySet.intersect(l.keySet)
          date -> innerKeys.foldLeft(Map.empty[Long, Double])((inner: Map[Long, Double], id: Long) => {
            inner + (id -> w(id) / (w(id) + l(id)))
          })
        }
      }.toMap)
    }) match {
      case Some(pair) => result + pair
      case None => result
    }
  }

  def baseModel = new AccumulatorModel {
    def key: String = "inner-win-loss"


    def accumulators: List[AccumulativeStatistic] = List(
      new AccumulativeStatistic {
        val key = "wins"

        def accumulateDate(obs: ScheduleData, data: Map[Long, Double]): Map[Long, Double] = {
          obs.winner match {
            case Some(t) => data + (t.id -> (data.getOrElse(t.id, 0.0) + 1.0))
            case None => data
          }
        }
      },

      new AccumulativeStatistic {
        val key = "losses"

        def accumulateDate(obs: ScheduleData, data: Map[Long, Double]): Map[Long, Double] = {
          obs.loser match {
            case Some(t) => data + (t.id -> (data.getOrElse(t.id, 0.0) + 1.0))
            case None => data
          }
        }
      },

      new AccumulativeStatistic {
        val key = "streak"

        def accumulateDate(obs: ScheduleData, data: Map[Long, Double]): Map[Long, Double] = {
          val d = obs.winner match {
            case Some(t) => data + (t.id -> math.max(data.getOrElse(t.id, 0.0) + 1.0, 1.0))
            case None => data
          }
          obs.loser match {
            case Some(t) => d + (t.id -> math.min(d.getOrElse(t.id, 0.0) - 1.0, -1.0))
            case None => d
          }
        }
      }
    )

    def statistics: Map[String, Statistic] = List(
      Statistic(-1, "wins", "Wins", -1, "Team", "%d", "%d", higherIsBetter = true),
      Statistic(-1, "losses", "Losses", -1, "Team", "%d", "%d", higherIsBetter = false),
      Statistic(-1, "streak", "Streak", -1, "Team", "%d", "%d", higherIsBetter = true)

    ).map(s => s.key -> s).toMap
  }

  def statistics: Map[String, Statistic] = baseModel.statistics++List(
    Statistic(-1, "wp", "Winning Pct", -1, "Team", "%0.3f", "%0.5f", higherIsBetter = true)
  ).map(s => s.key -> s).toMap
}
