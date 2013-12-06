package analysis

import org.joda.time.LocalDate
import models.{Statistic, ScheduleData}
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

class ScoringModel extends AccumulatorModel {
  def key = "scoring"

  def statistics: Map[String, Statistic] = List(
    Statistic(-1, "mean-points-for", "Mean Points For", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 1),
    Statistic(-1, "mean-points-against", "Mean Points Against", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 2),
    Statistic(-1, "mean-points-margin", "Mean Points Margin", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 3),
    Statistic(-1, "mean-points-total", "Mean Points Total", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 4),
    Statistic(-1, "stddev-points-for", "Std Dev Points For", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 5),
    Statistic(-1, "stddev-points-against", "Std Dev Points Against", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 6),
    Statistic(-1, "stddev-points-margin", "Std Dev Points Margin", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 7),
    Statistic(-1, "stddev-points-total", "Std Dev Points Total", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 8),
    Statistic(-1, "max-points-for", "Max Points For", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 9),
    Statistic(-1, "max-points-against", "Max Points Against", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 10),
    Statistic(-1, "max-points-margin", "Max Points Margin", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 11),
    Statistic(-1, "max-points-total", "Max Points Total", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 12),
    Statistic(-1, "min-points-for", "Min Points For", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 13),
    Statistic(-1, "min-points-against", "Min Points Against", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 14),
    Statistic(-1, "min-points-margin", "Min Points Margin", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 15),
    Statistic(-1, "min-points-total", "Min Points Total", -1, "Team", "%6.3f", "%6.3f", higherIsBetter = true, 16)
  ).map(s => s.key -> s).toMap

  def mean(ds: DescriptiveStatistics): Double = ds.getMean

  def min(ds: DescriptiveStatistics): Double = ds.getMin

  def max(ds: DescriptiveStatistics): Double = ds.getMax

  def stddev(ds: DescriptiveStatistics): Double = ds.getStandardDeviation

  def accumulators: List[AccumulativeStatistic[_]] = List(
    AccumulativeStatistic[DescriptiveStatistics](Map("mean-points-for" -> mean, "stddev-points-for" -> stddev, "min-points-for" -> min, "max-points-for" -> max), (obs, data) => {
      val h: Long = obs.homeTeam.id
      val hds = data.getOrElse(h, new DescriptiveStatistics())
      hds.addValue(obs.result.get.homeScore)
      val a: Long = obs.awayTeam.id
      val ads = data.getOrElse(a, new DescriptiveStatistics())
      ads.addValue(obs.result.get.awayScore)
      data + (h -> hds) + (a -> ads)
    }),

    AccumulativeStatistic[DescriptiveStatistics](Map("mean-points-against" -> mean, "stddev-points-against" -> stddev, "min-points-against" -> min, "max-points-against" -> max), (obs, data) => {
      val h: Long = obs.homeTeam.id
      val hds = data.getOrElse(h, new DescriptiveStatistics())
      hds.addValue(obs.result.get.awayScore)
      val a: Long = obs.awayTeam.id
      val ads = data.getOrElse(a, new DescriptiveStatistics())
      ads.addValue(obs.result.get.homeScore)
      data + (h -> hds) + (a -> ads)
    }),

    AccumulativeStatistic[DescriptiveStatistics](Map("mean-points-margin" -> mean, "stddev-points-margin" -> stddev, "min-points-margin" -> min, "max-points-margin" -> max), (obs, data) => {
      val h: Long = obs.homeTeam.id
      val hds = data.getOrElse(h, new DescriptiveStatistics())
      hds.addValue(obs.result.get.homeScore - obs.result.get.awayScore)
      val a: Long = obs.awayTeam.id
      val ads = data.getOrElse(a, new DescriptiveStatistics())
      ads.addValue(obs.result.get.awayScore - obs.result.get.homeScore)
      data + (h -> hds) + (a -> ads)
    }),

    AccumulativeStatistic[DescriptiveStatistics](Map("mean-points-total" -> mean, "stddev-points-total" -> stddev, "min-points-total" -> min, "max-points-total" -> max), (obs, data) => {
      val h: Long = obs.homeTeam.id
      val hds = data.getOrElse(h, new DescriptiveStatistics())
      hds.addValue(obs.result.get.homeScore + obs.result.get.awayScore)
      val a: Long = obs.awayTeam.id
      val ads = data.getOrElse(a, new DescriptiveStatistics())
      ads.addValue(obs.result.get.awayScore + obs.result.get.homeScore)
      data + (h -> hds) + (a -> ads)
    })
  )
}
