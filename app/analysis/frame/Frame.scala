package analysis.frame

import org.apache.commons.math.stat.descriptive.DescriptiveStatistics
import analysis._

case class Frame[O, U, X](data: Map[O, Map[U, X]] = Map.empty[O, Map[U, X]])(implicit ord: Ordering[O], num: Numeric[X]) {
  self: Frame[O, U, X] =>


  def add(o: O, u: U, x: X): Frame[O, U, X] = Frame(data + (o -> (data.getOrElse(o, Map.empty[U, X]) + (u -> x))))

  def addAll(list: List[(O, U, X)]) = Frame(list.foldLeft(data)((d: (Map[O, Map[U, X]]), tup: (O, U, X)) => d + (tup._1 -> (data.getOrElse(tup._1, Map.empty[U, X]) + (tup._2 -> tup._3)))))

  val ordering: List[O] = data.keySet.toList.sorted
  val ids: Set[U] = data.values.foldLeft(Set.empty[U])((set: Set[U], map: Map[U, X]) => set ++ map.keySet)
  val rankedSets: Map[O, List[X]] = ordering.map(k => k -> data.getOrElse(k, Map.empty[U, X]).values.toList.sortBy(x => num.toDouble(x))).toMap
  val stats: Map[O, DescriptiveStatistics] = ordering.map(k => k -> new DescriptiveStatistics(rankedSets(k).map(x => num.toDouble(x)).toArray)).toMap

  def population(o: O): Population[U, X] = {
    require(!data.isEmpty)
    new Population[U, X] {


      override def ids: Set[U] = data(o).keySet

      override def count: Int = rankedSets(o).size

      override def skewness: Double = stats(o).getSkewness

      override def median: Double = stats(o).getPercentile(0.50)

      override def q3: Double = stats(o).getPercentile(0.75)

      override def q1: Double = stats(o).getPercentile(0.25)

      override def mean: Double = stats(o).getMean

      override def kurtosis: Double = stats(o).getKurtosis

      override def minimum: Double = stats(o).getMin

      override def maximum: Double = stats(o).getMax

      override def stdDev: Double = stats(o).getStandardDeviation

      override def rank(u: U, ties: TieMethod): Option[Double] = value(u).map((v: X) => {
        val first = rankedSets(o).indexWhere(_ == v)
        val last = rankedSets(o).indexWhere(_ != v, first) - 1
        ties match {
          case HighRank => first
          case LowRank => last
          case Average => (first + last) / 2
        }

      }
      )

      override def percentile(u: U): Option[Double] = rank(u, Average).map(rk => (count - rk) / count)

      override def value(u: U): Option[X] = data(o).get(u)

      override def zScore(u: U): Option[Double] = value(u).map(v => (num.toDouble(v) - mean) / stdDev)
    }
  }

  def series(u: U): Series[O, X] = new Series[O, X] {
    require(!data.isEmpty)
    val subKeyList = ordering.filter(k => data(k).contains(u))

    override def count: Int = subKeyList.size

    override def firstKey: Option[O] = subKeyList.headOption

    override def lastKey: Option[O] = subKeyList.lastOption

    override def maxKey: List[O] = subKeyList.filter(k => data(k)(u) == subKeyList.flatMap(d => value(d)).max)

    override def minKey: List[O] = subKeyList.filter(k => data(k)(u) == subKeyList.flatMap(d => value(d)).min)

    override def last: Option[X] = lastKey.flatMap(k => value(k))

    override def value(o: O): Option[X] = data.get(o).flatMap(_.get(u))

    override def keys: List[O] = subKeyList

    override def first: Option[X] = firstKey.flatMap(k => value(k))

    override def rank(o: O, ties: TieMethod): Option[Double] = population(o).rank(u, ties)

    override def percentile(o: O): Option[Double] = population(o).percentile(u)

    override def zScore(o: O): Option[Double] = population(o).zScore(u)

    override implicit def ord: Ordering[O] = self.ord
  }

}
