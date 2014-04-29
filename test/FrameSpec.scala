import analysis.Frame
import org.joda.time.LocalDate
import org.specs2.mutable.Specification

class FrameSpec extends Specification {

  import models.util.Mappers.dateTimeOrdering

  "An empty frame" should {
    "have an empty ordering" in {
      Frame[Double, String, Double]().ordering.isEmpty
    }

    "have an empty idSet in " in {
      Frame[LocalDate, String, Double]().ids.isEmpty
    }

    "fail to return a series" in {
      Frame[LocalDate, String, Double]().series("A") must throwA[IllegalArgumentException]
    }

    "fail to return a population" in {
      Frame[LocalDate, String, Double]().population(new LocalDate("20140429")) must throwA[IllegalArgumentException]
    }

    "accept a new observation and return a new Frame" in {
      val f = Frame[LocalDate, String, Double]().add(new LocalDate("20140429"), "JIM", 100.0)
      f.ids must beEqualTo(Set("JIM"))
      f.ordering must beEqualTo(List(new LocalDate("20140429")))
    }

    "accept a list of observations and return a new Frame" in {
      val f = Frame[LocalDate, String, Double]().addAll(List((new LocalDate("20140429"), "JIM", 100.0)))
      f.ids must beEqualTo(Set("JIM"))
      f.ordering must beEqualTo(List(new LocalDate("20140429")))
    }

  }


}
