import sbt._
import play.Project._

object ApplicationBuild extends Build {

  val appName = "playfij"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "com.typesafe.slick" %% "slick" % "1.0.0",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "com.h2database" % "h2" % "1.3.166",
    "mysql" % "mysql-connector-java" % "5.1.5",
    "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2",
    "com.typesafe.play" %% "play-slick" % "0.4.0"


  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
