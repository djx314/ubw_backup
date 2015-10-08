import sbt._
import Keys._
import org.xarcher.sbt.CustomSettings
import com.typesafe.sbt.SbtGit._

object ubw extends Build {
  
  val initPrintln = """
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

  lazy val playVersion = play.core.PlayVersion.current
  val poiVersion = "3.13"

  lazy val ubw = (project in file("."))
  //play
  .enablePlugins(play.sbt.PlayScala)
  //play end
  //native packaged settings
  .enablePlugins(com.typesafe.sbt.packager.windows.WindowsPlugin)
  //native packaged settings end
  //common settings
  .settings(CustomSettings.customSettings: _*)
  .settings(
    name := "ubw",
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
      "jp.t2v" %% "play2-auth" % "0.14.1",
      "jp.t2v" %% "play2-auth-social" % "0.14.1",
      "jp.t2v" %% "play2-auth-test" % "0.14.1" % "test",
      play.sbt.Play.autoImport.cache

    )
  )
  //commonSettings end
  .dependsOn(`我要做个大新闻`)

  lazy val `我要做个大新闻` = (project in file("./ubw-core"))
  .settings(
    libraryDependencies ++= Seq(

      //slick
      "com.github.tminglei" %% "slick-pg" % "0.10.0-RC1",
      "com.typesafe.slick" %% "slick" % "3.1.0-RC2",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0-RC2" exclude("com.zaxxer", "HikariCP-java6"),
      "com.typesafe.play" %% "play-slick" % "1.1.0-RC2",
      "com.chuusai" %% "shapeless" % "2.2.5",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "com.h2database" % "h2" % "1.4.181" % "test",
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
  .settings(CustomSettings.scalaSettings ++ CustomSettings.resolversSettings)

}
