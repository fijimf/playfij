package analysis.junkyard


case class SpreadDist(name:String, n:Double, mean:Double, stDev:Double, skew:Double, kurtosis:Double, pctOnePtGames:Double, min:Double, q1:Double, med:Double, q3:Double, p95:Double, p99:Double, max:Double, mode:(List[Int], Int))
