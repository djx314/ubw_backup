import org.xarcher.sbt._

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

//net.virtualvoid.sbt.graph.Plugin.graphSettings
CustomSettings.customSettings

libraryDependencies ++= Seq(
  //repl
  "com.lihaoyi" % "ammonite-repl" % "0.4.8" % "test" cross CrossVersion.full,
  //slick
  "com.github.tminglei" %% "slick-pg" % "0.10.1",
  "com.github.tminglei" %% "slick-pg_jts" % "0.10.1",
  //"com.github.tminglei" %% "slick-pg_play-json" % "0.10.1",
  "com.typesafe.slick" %% "slick" % "3.1.0",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0" exclude("com.zaxxer", "HikariCP-java6"),
  //"com.typesafe.play" %% "play-slick" % "1.1.0",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.h2database" % "h2" % "1.4.181" % "test",
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "org.joda" % "joda-convert" % "1.7",
  "com.vividsolutions" % "jts" % "1.13",
  //scalaz
  "org.scalaz" %% "scalaz-core" % "7.2.0-M5",
  "org.scalaz" %% "scalaz-effect" % "7.2.0-M5",
  "org.scalaz" %% "scalaz-iteratee" % "7.2.0-M5",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.0-M5",
  //parboiled
  "org.parboiled" %% "parboiled" % "2.1.0"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)