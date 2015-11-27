import org.xarcher.sbt._

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

//net.virtualvoid.sbt.graph.Plugin.graphSettings
CustomSettings.customSettings

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

lazy val `ubw` = (project in file("."))
.dependsOn(`core`)
.aggregate(`core`)

lazy val `core` = (project in file("./ubw-core"))