import analysis.frame.Frame
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
      Frame[LocalDate, String, Double]().population(new LocalDate("2014-04-29")) must throwA[IllegalArgumentException]
    }

    "accept a new observation and return a new Frame" in {
      val f = Frame[LocalDate, String, Double]().add(new LocalDate("2014-04-29"), "JIM", 100.0)
      f.ids must beEqualTo(Set("JIM"))
      f.ordering must beEqualTo(List(new LocalDate("2014-04-29")))
    }

    "accept a list of observations and return a new Frame" in {
      val f = Frame[LocalDate, String, Double]().addAll(List((new LocalDate("2014-04-29"), "JIM", 100.0)))
      f.ids must beEqualTo(Set("JIM"))
      f.ordering must beEqualTo(List(new LocalDate("2014-04-29")))
    }

  }

  "Constructing a frame from a map" should {
    val f = Frame(Map(
      new LocalDate("2014-04-29") -> Map("A" -> 2.0, "C" -> 3.0, "D" -> 5.0),
      new LocalDate("2014-04-28") -> Map("A" -> 1.0, "B" -> 2.0, "C" -> 3.0, "D" -> 4.0),
      new LocalDate("2014-04-30") -> Map("A" -> 3.0, "B" -> 4.0, "C" -> 4.0, "E" -> 10.0))
    )
    "result in a data which matches the dimensions of the map" in {
      f.ordering.size must beEqualTo(3)
      f.ids.size must beEqualTo(5)
    }

    "result in an ordering which represents the unique sorted ordered keys" in {
      f.ordering must beEqualTo(List(new LocalDate("2014-04-28"), new LocalDate("2014-04-29"), new LocalDate("2014-04-30")))
    }

    "result in ids which represent the union of all ids" in {
      f.ids must beEqualTo(Set("A", "B", "C", "D", "E"))
    }

  }

  "A series constructed from a Frame" should {
    val f = Frame(Map(
      new LocalDate("2014-04-29") -> Map("A" -> 2.0, "C" -> 3.0, "D" -> 5.0),
      new LocalDate("2014-04-28") -> Map("A" -> 1.0, "B" -> 2.0, "C" -> 3.0, "D" -> 4.0),
      new LocalDate("2014-04-30") -> Map("A" -> 3.0, "B" -> 4.0, "C" -> 4.0, "E" -> 10.0))
    )

    "Should only have keys size representative of the ids" in {
      f.series("A").count must beEqualTo(3)
      f.series("B").count must beEqualTo(2)
      f.series("C").count must beEqualTo(3)
      f.series("D").count must beEqualTo(2)
      f.series("E").count must beEqualTo(1)
    }

    "Should only have ordered keys representative of the ids" in {
      f.series("A").keys must beEqualTo(List(new LocalDate("2014-04-28"), new LocalDate("2014-04-29"), new LocalDate("2014-04-30")))
      f.series("B").keys must beEqualTo(List(new LocalDate("2014-04-28"), new LocalDate("2014-04-30")))
      f.series("C").keys must beEqualTo(List(new LocalDate("2014-04-28"), new LocalDate("2014-04-29"), new LocalDate("2014-04-30")))
      f.series("D").keys must beEqualTo(List(new LocalDate("2014-04-28"), new LocalDate("2014-04-29")))
      f.series("E").keys must beEqualTo(List(new LocalDate("2014-04-30")))
    }

    "Should have first and last keys representative of the ids" in {
      f.series("A").firstKey.get must beEqualTo(new LocalDate("2014-04-28"))
      f.series("A").lastKey.get must beEqualTo(new LocalDate("2014-04-30"))
      f.series("B").firstKey.get must beEqualTo(new LocalDate("2014-04-28"))
      f.series("B").lastKey.get must beEqualTo(new LocalDate("2014-04-30"))
      f.series("C").firstKey.get must beEqualTo(new LocalDate("2014-04-28"))
      f.series("C").lastKey.get must beEqualTo(new LocalDate("2014-04-30"))
      f.series("D").firstKey.get must beEqualTo(new LocalDate("2014-04-28"))
      f.series("D").lastKey.get must beEqualTo(new LocalDate("2014-04-29"))
      f.series("E").firstKey.get must beEqualTo(new LocalDate("2014-04-30"))
      f.series("E").lastKey.get must beEqualTo(new LocalDate("2014-04-30"))
    }

    "Should have first and last values representative of the ids" in {
      f.series("A").first.get must beEqualTo(1.0)
      f.series("A").last.get must beEqualTo(3.0)
      f.series("B").first.get must beEqualTo(2.0)
      f.series("B").last.get must beEqualTo(4.0)
      f.series("C").first.get must beEqualTo(3.0)
      f.series("C").last.get must beEqualTo(4.0)
      f.series("D").first.get must beEqualTo(4.0)
      f.series("D").last.get must beEqualTo(5.0)
      f.series("E").first.get must beEqualTo(10.0)
      f.series("E").last.get must beEqualTo(10.0)

    }

    "Should have appropriate max and min keys" in {
      f.series("A").minKey must beEqualTo(List(new LocalDate("2014-04-28")))
      f.series("A").maxKey must beEqualTo(List(new LocalDate("2014-04-30")))
      f.series("B").minKey must beEqualTo(List(new LocalDate("2014-04-28")))
      f.series("B").maxKey must beEqualTo(List(new LocalDate("2014-04-30")))
      f.series("C").minKey must beEqualTo(List(new LocalDate("2014-04-28"),new LocalDate("2014-04-29")))
      f.series("C").maxKey must beEqualTo(List(new LocalDate("2014-04-30")))
      f.series("D").minKey must beEqualTo(List(new LocalDate("2014-04-28")))
      f.series("D").maxKey must beEqualTo(List(new LocalDate("2014-04-29")))
      f.series("E").minKey must beEqualTo(List(new LocalDate("2014-04-30")))
      f.series("E").maxKey must beEqualTo(List(new LocalDate("2014-04-30")))

    }

  }
  //  "Adding an element to a frame" should {
  //    "should not allow duplicate ordered keys" in {
  //
  //    }
  //
  //    "should not allow duplicate unordered keys" in {
  //
  //    }
  //  }

}
