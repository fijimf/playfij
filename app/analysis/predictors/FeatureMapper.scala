package analysis.predictors
import org.apache.mahout.math.Vector
import models.ScheduleData

trait FeatureMapper {
  def featureDimension:Int
  def feature(obs:ScheduleData):Option[Vector]
}
