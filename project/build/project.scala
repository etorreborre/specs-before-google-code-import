import sbt._
import webbytest.HtmlTestsProject

class Project(info: ProjectInfo) extends DefaultProject(info) with HtmlTestsProject {

  val mavenLocal = "Local Maven Repository" at "file:///D:/mvn-repository"
  override def outputDirectoryName = "build"
  override def managedDependencyPath = "project" / "lib_managed"
  override def compileOptions = Unchecked :: super.compileOptions.toList
  override def javaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m -Xss1M") :: Nil
  override def testJavaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m") :: Nil
  override def includeTest(s: String) = { s.endsWith("Spec") || s.endsWith("Unit") }
  val junit 		= "junit" % "junit" % "4.5"
  val wikitext 		= "org.eclipse.mylyn.wikitext" % "wikitext" % "0.9.4.I20090220-1600-e3x" 
  val wikitextile 	= "org.eclipse.mylyn.wikitext" % "wikitext.textile" % "0.9.4.I20090220-1600-e3x" 
  val scalatest 	= "org.scalatest" % "scalatest" % "1.0"
  val scalacheck 	= "org.scala-tools.testing" %% "scalacheck" % "1.6" 
  val jmock 		= "org.jmock" % "jmock" % "2.4.0" 
  val easymock 		= "org.easymock" % "easymock" % "2.5.1" 
  val easymockclass	= "org.easymock" % "easymockclassextension" % "2.4" 
  val mockito 		= "org.mockito" % "mockito-all" % "1.8.4" 
  val cglib 		= "cglib" % "cglib" % "2.1_3"  
  val cglibnodep	= "cglib" % "cglib-nodep" % "2.1_3"  
  val objenesis 	= "org.objenesis" % "objenesis" % "1.0"
  val testInterfaces = "org.scala-tools.testing" % "test-interface" % "0.2"

  val scriptapi 	= "javax.script" % "script-api" % "1.0"
  val scriptjs   	= "javax.script" % "script-js" % "1.0"
  val jsengine  	= "javax.script" % "js-engine" % "1.0"

  //override def crossScalaVersions = List("2.7.7")

  val publishTo = "Scala Tools Nexus" at "http://nexus-direct.scala-tools.org/content/repositories/snapshots/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  val snapshotsRepo = Resolver.url("snapshots-repo", new java.net.URL("http://nexus-direct.scala-tools.org/content/repositories/snapshots"))
  val specsRepo = Resolver.url("specs-repo", new java.net.URL("http://specs.googlecode.com/svn/maven2"))

}
