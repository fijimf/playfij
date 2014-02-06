import sbt._
import play.Project._
import Keys._

object ApplicationBuild extends Build {

  val appName = "playfij"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    cache,
    anorm,
  "com.kenshoo" %% "metrics-play" % "0.1.3",
  "com.typesafe.slick" %% "slick" % "1.0.1",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "com.h2database" % "h2" % "1.3.166",
    "mysql" % "mysql-connector-java" % "5.1.5",
    "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2",
    "com.google.guava" % "guava" % "14.0",
    "com.typesafe.play" %% "play-slick" % "0.5.0.8" exclude("org.scala-stm", "scala-stm_2.10.0") exclude("play", "*"),
    "securesocial" %% "securesocial" % "2.1.2" exclude("org.scala-stm", "scala-stm_2.10.0") exclude("play", "*"),
    "org.scala-saddle" %% "saddle-core" % "1.3.+",
    "org.apache.mahout" % "mahout-math" % "0.8",
    "org.apache.mahout" % "mahout-core" % "0.8"

  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalaVersion := "2.10.2",
    resolvers += Resolver.url("sbt-plugin-snapshots", url("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
  )

}
