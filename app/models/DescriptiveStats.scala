package models

import org.saddle.Series
import org.saddle.stats.PctMethod

case class DescriptiveStats(n:Int, mean:Double, stdDev:Double, skew:Double, kurt:Double, min:Double, q1:Double, med:Double, q3:Double, max:Double)

object DescriptiveStats {
  def apply(series: Series[_, Double]): DescriptiveStats = {
    (series.length, series.min, series.max) match {
      case (n, Some(min), Some(max)) => DescriptiveStats(n, series.mean, series.stdev, series.skew, series.kurt, min, series.percentile(25.0, PctMethod.NIST), series.median, series.percentile(75.0, PctMethod.NIST), max)
      case (n, _, _) => DescriptiveStats(0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN)
    }
  }
}
