package analysis

import org.joda.time.LocalDate
import models.Team
import org.apache.commons.math.stat.descriptive.{DescriptiveStatistics, UnivariateStatistic}

sealed trait TieMethod

case object HighRank extends TieMethod

case object LowRank extends TieMethod

case object Average extends TieMethod

trait Population[U, X <: Numeric] {

  def count: Int

  def minimum: Double

  def median: Double

  def maximum: Double

  def mean: Double

  def stdDev: Double

  def skewness: Double

  def kurtosis: Double

  def value(u: U): Option[X]

  def rank(u: U, ties: TieMethod): Option[Double]

  def zScore(u: U): Option[Double]

  def percentile(u: U): Option[Double]

}

trait Series[O, X <: Numeric] {
  def count: Int

  def firstKey: O

  def lastKey: O

  def first: X

  def last: X

  def keys: List[O]

  def values: List[X]

  def value(o: O)
}

case class Frame[O, U, X <: Numeric[X]](data: Map[O, Map[U, X]]) {


  def population(o: O): Population[U, X] = new Population[U, X] {
    val p = data.getOrElse(o, Map.empty[U, X])
    val x = p.values.toList.sorted
    val stats = new DescriptiveStatistics(x.map(xx => xx.toDouble(xx)).toArray)

    override def count: Int = p.size

    override def skewness: Double = stats.getSkewness


    override def median: Double = stats.getPercentile(0.50)

    override def mean: Double = stats.getMean

    override def kurtosis: Double = stats.getKurtosis

    override def minimum: Double = stats.getMin

    override def maximum: Double = stats.getMax

    override def stdDev: Double = stats.getStandardDeviation

    override def rank(u: U, ties: TieMethod): Option[Double] = value(u).map((v: X) => {
      val first = x.indexWhere(_ == v)
      val last = x.indexWhere(_ != v, first) - 1
      ties match {
        case HighRank => first
        case LowRank => last
        case Average => (first + last) / 2
      }

    }
    )

    override def percentile(u: U): Option[Double] = rank(u, Average).map(rk => (count - rk) / count)

    override def value(u: U): Option[X] = p.get(u)

    override def zScore(u: U): Option[Double] = value(u).map(v => (v.toDouble(v) - mean) / stdDev)
  }

  def series(u: U): Series[O, X] = new Series[O,X]{
    override def count: Int = ???

    override def firstKey: O = ???

    override def lastKey: O = ???

    override def values: List[X] = ???

    override def last: X = ???

    override def value(o: O): Unit = ???

    override def keys: List[O] = ???

    override def first: X = ???
  }

}
