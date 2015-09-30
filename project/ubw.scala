import sbt._
import Keys._
import org.xarcher.sbt.CustomSettings
import com.typesafe.sbt.SbtGit._

object enuma extends Build {
  
  val initPrintln = """
                                                     _   _         _     
                                                    | | (_)       | |    
  ___   _ __    _   _   _ __ ___     __ _      ___  | |  _   ___  | |__  
 / _ \ | '_ \  | | | | | '_ ` _ \   / _` |    / _ \ | | | | / __| | '_ \ 
|  __/ | | | | | |_| | | | | | | | | (_| |   |  __/ | | | | \__ \ | | | |
 \___| |_| |_|  \__,_| |_| |_| |_|  \__,_|    \___| |_| |_| |___/ |_| |_|
"""
  println(initPrintln)

  lazy val playVersion = play.core.PlayVersion.current
  val poiVersion = "3.13"

  lazy val enuma = (project in file("."))
  //play
  .enablePlugins(play.sbt.PlayScala)
  //play end
  //native packaged settings
  .enablePlugins(com.typesafe.sbt.packager.windows.WindowsPlugin)
  //native packaged settings end
  //common settings
  .settings(CustomSettings.customSettings: _*)
  .settings(
    name := "enuma",
    version := "0.0.1",
    if (org.xarcher.sbt.OSName.isWindows)
      initialCommands in console += """ammonite.repl.Repl.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");"""
    else if (org.xarcher.sbt.OSName.isLinux)
      initialCommands in console += """ammonite.repl.Repl.run("");"""
    else
      initialCommands in console += """""",
    libraryDependencies ++= Seq(

      //repl
      "com.lihaoyi" % "ammonite-repl" % "0.4.8" cross CrossVersion.full,

      //webjars
      "org.webjars" % "json2" % "20140204",
      "org.webjars" %% "webjars-play" % "2.4.0-1",

      //play-jdbc
      "com.typesafe.play" %% "play-jdbc" % playVersion,

      //play-json-extionsions
      "org.cvogt" %% "play-json-extensions" % "0.5.0",

      //poi
      "org.apache.poi" % "poi" % poiVersion exclude("stax", "stax-api"),
      "org.apache.poi" % "poi-ooxml" % poiVersion exclude("stax", "stax-api"),
      "org.apache.poi" % "poi-ooxml-schemas" % poiVersion exclude("stax", "stax-api"),

      //zip
      "net.lingala.zip4j" % "zip4j" % "1.3.2",

      //security
      //"be.objectify" %% "deadbolt-scala" % "2.4.1.1" exclude("com.google.code.findbugs", "jsr305"),
      "jp.t2v" %% "play2-auth" % "0.14.1",
      "jp.t2v" %% "play2-auth-social" % "0.14.1",
      "jp.t2v" %% "play2-auth-test" % "0.14.1" % "test",
      play.sbt.Play.autoImport.cache,

      //slick
      /*"io.strongtyped" %% "active-slick" % "0.3.1",
      "com.github.tminglei" %% "slick-pg" % "0.10.0-RC1",
      "com.typesafe.slick" %% "slick" % "3.1.0-RC1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0-RC1" exclude("com.zaxxer", "HikariCP-java6"),
      "com.typesafe.play" %% "play-slick" % "1.1.0-M2",*/

      /*"io.strongtyped" %% "active-slick" % "0.3.1",
      "com.github.tminglei" %% "slick-pg" % "0.9.2",
      "com.typesafe.slick" %% "slick" % "3.0.3",
      "com.typesafe.play" %% "play-slick" % "1.0.1",*/

      "org.apache.commons" % "commons-lang3" % "3.3.2",
      "org.joda" % "joda-convert" % "1.7",
      "com.vividsolutions" % "jts" % "1.13",

      //future extionsions
      "com.thoughtworks.each" %% "each" % "0.4.1",

      //scalaz
      "org.scalaz" %% "scalaz-core" % "7.2.0-M3",
      "org.scalaz" %% "scalaz-effect" % "7.2.0-M3",
      "org.scalaz" %% "scalaz-iteratee" % "7.2.0-M3",
      "org.scalaz" %% "scalaz-concurrent" % "7.2.0-M3"

    )
  )
  //commonSettings end
  .dependsOn(`active-slick-modules-shapeless`)

  lazy val `active-slick-modules-shapeless` = (project in file("./active-slick/modules/shapeless"))
  .settings(CustomSettings.scalaSettings ++ CustomSettings.resolversSettings)
  .dependsOn(`active-slick-modules-core`)

  lazy val `active-slick-modules-core` = (project in file("./active-slick/modules/core"))
  .settings(
    libraryDependencies ++= Seq(
      //slick
      "com.github.tminglei" %% "slick-pg" % "0.10.0-RC1",
      "com.typesafe.slick" %% "slick" % "3.1.0-RC2",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0-RC2" exclude("com.zaxxer", "HikariCP-java6"),
      "com.typesafe.play" %% "play-slick" % "1.1.0-RC2",
      "com.chuusai" %% "shapeless" % "2.2.5",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "com.h2database" % "h2" % "1.4.181" % "test"
    )
  )
  .settings(CustomSettings.scalaSettings ++ CustomSettings.resolversSettings)

}
