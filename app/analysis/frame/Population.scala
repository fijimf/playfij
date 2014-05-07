package analysis.frame

trait Population[U, X] {

  def ids:Set[U]

  def count: Int

  def minimum: Double

  def q1: Double

  def median: Double

  def q3: Double

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
