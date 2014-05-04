package analysis.frame

trait Population[U, X] {

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
