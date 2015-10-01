package org.xarcher.sbt

import sbt._
import Keys._

object CustomSettings {
  
  def customSettings = scalaSettings ++ resolversSettings ++ extAlias ++ playSettings ++ graphSettings ++ assemblyPluginSettings ++ nativePackageSettings
  
  def scalaSettings =
    Seq(
      scalaVersion := "2.11.7",
      scalacOptions ++= Seq("-feature", "-deprecation"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
    )
  
  def resolversSettings =
    Seq(
      resolvers ++= Seq(
        "mavenRepoJX" at "http://repo1.maven.org/maven2/",
        "bintray/non" at "http://dl.bintray.com/non/maven",
        "aa" at "https://oss.sonatype.org/service/local/repositories/snapshots/content/",
        Resolver.url("typesafe-ivy", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
      ),
      externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)
    )
  
  def extAliasInfo = List(
    Option("xeclipse" -> "eclipse with-source=true skip-parents=false"),
    Option("s" -> "我要做个大新闻/test"),
    if (OSName.isWindows)
      Option(windowsGitInitCommandMap)
    else if (OSName.isLinux)
      Option(linuxGitInitCommandMap)
    else None
  )

  def extAlias = extAliasInfo.collect { case Some(s) => s }
    .foldLeft(List.empty[Def.Setting[_]]){ (s, t) => s ++ addCommandAlias(t._1, t._2) }
  
  //git init command
  val windowsGitInitCommandMap = "windowsGitInit" ->
    """|;
        |git config --global i18n.commitencoding utf-8;
        |git config --global i18n.logoutputencoding gbk;
        |git config --global core.autocrlf true;
        |git config core.editor \"extras/npp.6.5.1/startNote.bat\"
      """.stripMargin

  val linuxGitInitCommandMap = "linuxGitInit" ->
    """|;
        |git config --global i18n.commitencoding utf-8;
        |git config --global i18n.logoutputencoding utf-8;
        |git config --global core.autocrlf true;
        |git config core.editor gedit
      """.stripMargin

  val playSettings = {

    import com.typesafe.sbt.less.Import._
    import com.typesafe.sbt.web.Import._
    import play.sbt.routes.RoutesKeys._
    import play.twirl.sbt.Import._

    Seq(
      routesImport ++= Seq("scala.language.reflectiveCalls"),
      routesGenerator := InjectedRoutesGenerator,
      com.typesafe.sbt.jse.JsEngineImport.JsEngineKeys.engineType := com.typesafe.sbt.jse.JsEngineImport.JsEngineKeys.EngineType.Trireme,
      TwirlKeys.templateImports ++= Seq("scalaz._", "Scalaz._"),
      includeFilter in (Assets, LessKeys.less) := "*.less",
      excludeFilter in (Assets, LessKeys.less) := "_*.less"
    )

  }

  val graphSettings = net.virtualvoid.sbt.graph.Plugin.graphSettings

  val assemblyPluginSettings = {

    import sbtassembly.{MergeStrategy, PathList}
    import sbtassembly.AssemblyKeys._

    sbtassembly.AssemblyPlugin.assemblySettings.++(
    Seq(
      mainClass in assembly := Some("play.core.server.NettyServer"),
      fullClasspath in assembly += Attributed.blank(play.sbt.PlayImport.PlayKeys.playPackageAssets.value),
      assemblyMergeStrategy in assembly <<= (assemblyMergeStrategy in assembly) { old => {
        //case "application.conf" => MergeStrategy.concat
        case PathList("META-INF", "spring.tooling") => MergeStrategy.discard
        case x => old(x)
      } })
    )

  }

  val nativePackageSettings = {

    import com.typesafe.sbt.SbtNativePackager.autoImport._
    import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._
    import com.typesafe.sbt.packager.windows.WindowsPlugin.autoImport._

    Seq(
      mappings in Windows := (mappings in Universal).value,
      // general package information (can be scoped to Windows)
      maintainer := "djx314<djx314@sina.cn>",
      packageSummary := "enuma-windows",
      packageDescription := """Enuma Elish.""",
      // wix build information
      wixProductId in Windows := "ce07be71-510d-414a-92d4-dff47631848a",
      wixProductUpgradeId in Windows := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"
    )

  }

}