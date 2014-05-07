package analysis.frame

trait Series[O, X] {

  implicit def ord: Ordering[O]

  def count: Int

  def minKey: List[O]

  def maxKey: List[O]

  def firstKey: Option[O]

  def lastKey: Option[O]

  def first: Option[X]

  def last: Option[X]

  def keys: List[O]

  def value(o: O):Option[X]

  def rank(o: O, ties: TieMethod): Option[Double]

  def zScore(o: O): Option[Double]

  def percentile(o: O): Option[Double]

}
