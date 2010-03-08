import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val webbytest = "org.fusesource" % "webbytest" % "1.0-SNAPSHOT"
}