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
      "wp" -> outerKeys.foldLeft(Map.empty[LocalDate, Map[Long, Double]])((outer: Map[LocalDate, Map[Long, Double]], date: LocalDate) => {
        (for (w <- wins.get(date);
              l <- losses.get(date)) yield {
          val innerKeys: Set[Long] = w.keySet.intersect(l.keySet)
          date -> innerKeys.foldLeft(Map.empty[Long, Double])((inner: Map[Long, Double], id: Long) => {
            inner + (id -> w(id) / (w(id) + l(id)))
          })
        })
        match {
          case Some(pair) => outer + pair
          case _ => outer
        }
      })
    }) match {
      case Some(pair) => result + pair
      case _ => result
    }
  }

  def baseModel = new AccumulatorModel {
    def key: String = "inner-win-loss"

    def ident(x: Double) = x


    def accumulators: List[AccumulativeStatistic[_]] = List(
      AccumulativeStatistic[Double](Map("wins" -> ident), (obs, data) => {
        val d = obs.winner match {
          case Some(t) => data + (t.id -> (data.getOrElse(t.id, 0.0) + 1.0))
          case None => data
        }
        obs.loser match {
          case Some(t) => d + (t.id -> data.getOrElse(t.id, 0.0))
          case None => d
        }
      }),

      AccumulativeStatistic[Double](Map("losses" -> ident), (obs, data) => {
        val d = obs.winner match {
          case Some(t) => data + (t.id -> data.getOrElse(t.id, 0.0))
          case None => data
        }
        obs.loser match {
          case Some(t) => d + (t.id -> (data.getOrElse(t.id, 0.0) + 1.0))
          case None => d
        }
      }),

      AccumulativeStatistic[Double](Map("streak" -> ident), (obs, data) => {
        val d = obs.winner match {
          case Some(t) => data + (t.id -> math.max(data.getOrElse(t.id, 0.0) + 1.0, 1.0))
          case None => data
        }
        obs.loser match {
          case Some(t) => d + (t.id -> math.min(d.getOrElse(t.id, 0.0) - 1.0, -1.0))
          case None => d
        }
      })
    )

    def statistics: Map[String, Statistic] = List(
      Statistic(-1, "wins", "Wins", -1, "Team", "%d", "%d", higherIsBetter = true, 1),
      Statistic(-1, "losses", "Losses", -1, "Team", "%d", "%d", higherIsBetter = false, 2),
      Statistic(-1, "streak", "Streak", -1, "Team", "%d", "%d", higherIsBetter = true, 3)

    ).map(s => s.key -> s).toMap
  }

  def statistics: Map[String, Statistic] = baseModel.statistics++List(
    Statistic(-1, "wp", "Winning Pct", -1, "Team", "%0.3f", "%0.5f", higherIsBetter = true, 4)
  ).map(s => s.key -> s).toMap
}
