package org.xarcher.ubw.sbt

import sbt._
import Keys._

object CustomSettings {
  
  def customSettings = extAlias ++ ammoniteConsoleInit

  def extAliasInfo = List(
    Option("xeclipse" -> "eclipse with-source=true skip-parents=false"),
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
        |git config core.editor \"extras/npp.6.5.1/startNote.bat\";
        |git config user.email "djx314@sina.cn";
        |git config user.name "djx314";
      """.stripMargin

  val linuxGitInitCommandMap = "linuxGitInit" ->
    """|;
        |git config --global i18n.commitencoding utf-8;
        |git config --global i18n.logoutputencoding utf-8;
        |git config --global core.autocrlf true;
        |git config core.editor gedit;
        |git config user.email "djx314@sina.cn";
        |git config user.name "djx314";
      """.stripMargin

  val ammoniteConsoleInit =
    if (OSName.isWindows)
      Seq(initialCommands in console += """ammonite.repl.Repl.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");""")
    else if (OSName.isLinux)
      Seq(initialCommands in console += """ammonite.repl.Repl.run("");""")
    else
      Seq()

}