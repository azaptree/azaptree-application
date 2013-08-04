import sbt._
import Keys._

object AzaptreeBuild extends Build {
  
  val projectId = "azaptree-application"

  println(String.format("*** Building %s",projectId))
  
  val akkaVersion = "2.2.0"

  lazy val root = Project(id = projectId, base = file("."))
   

}