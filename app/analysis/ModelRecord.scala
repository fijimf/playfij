package analysis

import org.saddle.scalar.Scalar
import org.saddle.stats.RankTie
import models.Statistic
import org.saddle.Series
import java.util.IllegalFormatConversionException


object ModelRecord {
  def fromStatValue(stat:Statistic, index:Int, ser:org.saddle.Series[_,Double]):ModelRecord={

    val value: Scalar[Double] = ser.at(index)
    val rank: Scalar[Double] = ser.rank(RankTie.Min, !stat.higherIsBetter).at(index)
    val z: Scalar[Double] = value.map(x => (x - ser.mean) / ser.stdev)
    ModelRecord(stat.name, stat.key, cleanString(value, stat.longFormat), cleanString(rank, "%.0f"), cleanString(z, "%4.2f"), stat.displayOrder)
  }

  def cleanString(x: Scalar[Double], format: String = "%5.2f"): String = {
    if (x.isNA) {
      "N/A"
    } else {
      try {
        format.format(x.get)
      }
      catch {
        case ex: IllegalFormatConversionException => if (ex.getConversion == 'd') {
          "%.0f".format(x.get)
        } else {
          "%f".format(x.get)
        }
      }
    }
  }
}
case class ModelRecord(name:String, key:String, value:String, rank:String, z:String, displayOrder:Int) {
  def rankString={
    rank match {
      case s if s.endsWith("11") => s+"th"
      case s if s.endsWith("12") => s+"th"
      case s if s.endsWith("13") => s+"th"
      case s if s.endsWith("1") => s+"st"
      case s if s.endsWith("2") => s+"nd"
      case s if s.endsWith("3") => s+"rd"
      case s => s+"th"
    }
  }

}
