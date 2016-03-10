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
scalaVersion := "2.11.8"}

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

libraryDependencies ++= {
  val slickVersion = "3.1.1"
  val circeV = "0.3.0"
  Seq(
    "io.circe" %% "circe-core" % circeV,
    "io.circe" %% "circe-generic" % circeV,
    "io.circe" %% "circe-parser" % circeV,
    //repl
    "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.5.6" % "test",
    //slick
    "com.github.tminglei" %% "slick-pg" % "0.11.2",
    "com.github.tminglei" %% "slick-pg_jts" % "0.11.2",
    "com.typesafe.slick" %% "slick" % slickVersion,
    "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6"),
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "com.h2database" % "h2" % "1.4.181"/*% "test"*/,
    "org.apache.commons" % "commons-lang3" % "3.3.2",
    "org.joda" % "joda-convert" % "1.7",
    "com.vividsolutions" % "jts" % "1.13",
    //slf4j
    "org.slf4j" % "slf4j-simple" % "1.7.13",
    //scalaz
    "org.scalaz" %% "scalaz-core" % "7.2.0",
    "org.scalaz" %% "scalaz-effect" % "7.2.0",
    "org.scalaz" %% "scalaz-iteratee" % "7.2.0",
    "org.scalaz" %% "scalaz-concurrent" % "7.2.0",
    //parboiled
    "org.parboiled" %% "parboiled" % "2.1.1"
  )
}

lazy val ubw = (project in file("."))
.dependsOn(`poi-collection`)

lazy val `poi-collection` = (project in file("./poi-collection"))