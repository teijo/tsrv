organization := "com.example"

name := "tsrv"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-directives" % "0.7.0",
  "net.databinder" %% "unfiltered-filter" % "0.7.0",
  "net.databinder" %% "unfiltered-jetty" % "0.7.0",
  "net.databinder" %% "unfiltered-spec" % "0.7.0" % "test",
  "org.json4s" %% "json4s-native" % "3.2.5",
  "org.scalaj" %% "scalaj-http" % "0.3.10"
)

resolvers ++= Seq(
  "java m2" at "http://download.java.net/maven/2"
)
