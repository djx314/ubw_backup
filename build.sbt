{val initPrintln = """
 _   _         _  _             _  _             _ 
| | | | _ __  | |(_) _ __ ___  (_)| |_  ___   __| |
| | | || '_ \ | || || '_ ` _ \ | || __|/ _ \ / _` |
| |_| || | | || || || | | | | || || |_|  __/| (_| |
 \___/ |_| |_||_||_||_| |_| |_||_| \__|\___| \__,_|

 ____   _             _       
| __ ) | |  __ _   __| |  ___ 
|  _ \ | | / _` | / _` | / _ \
| |_) || || (_| || (_| ||  __/
|____/ |_| \__,_| \__,_| \___|

__        __            _         
\ \      / /___   _ __ | | __ ___ 
 \ \ /\ / // _ \ | '__|| |/ // __|
  \ V  V /| (_) || |   |   < \__ \
   \_/\_/  \___/ |_|   |_|\_\|___/
"""
println(initPrintln)
scalaVersion := "2.11.7"}

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

libraryDependencies ++= Seq(
  //repl
  "com.lihaoyi" % "ammonite-repl" % "0.4.8" % "test" cross CrossVersion.full,
  //slick
  "com.github.tminglei" %% "slick-pg" % "0.10.1",
  "com.github.tminglei" %% "slick-pg_jts" % "0.10.1",
  "com.typesafe.slick" %% "slick" % "3.1.0",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0" exclude("com.zaxxer", "HikariCP-java6"),
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.h2database" % "h2" % "1.4.181" % "test",
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "org.joda" % "joda-convert" % "1.7",
  "com.vividsolutions" % "jts" % "1.13",
  //slf4j
  "org.slf4j" % "slf4j-simple" % "1.7.13",
  //scalaz
  "org.scalaz" %% "scalaz-core" % "7.2.0-M5",
  "org.scalaz" %% "scalaz-effect" % "7.2.0-M5",
  "org.scalaz" %% "scalaz-iteratee" % "7.2.0-M5",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.0-M5",
  //parboiled
  "org.parboiled" %% "parboiled" % "2.1.0"
)

lazy val ubw = (project in file("."))
.dependsOn(`play-caster`)
.dependsOn(`poi-collection`)

lazy val `play-caster` = (project in file("./play-caster"))

lazy val `poi-collection` = (project in file("./poi-collection"))
