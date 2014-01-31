package analysis.junkyard


case class SpreadDist(name:String, n:Int, mean:Double, stDev:Double, skew:Double, pctOnePtGames:Double, min:Int, q1:Double, med:Double, q3:Double, p95:Double, p99:Double, max:Int, mode:(List[Int], Int))
