import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val mavenLocal = "Local Maven Repository" at "file://D:/mvn-repository"
  override def outputDirectoryName = "build"
  override def compileOptions = Unchecked :: super.compileOptions.toList
  override def javaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m -Xss1M") :: Nil
  override def testJavaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m") :: Nil
  override def includeTest(s: String) = { s.endsWith("Spec") || s.endsWith("Unit") }
  val junit 		= "junit" % "junit" % "4.5"
  val wikitext 		= "org.eclipse.mylyn.wikitext" % "wikitext" % "0.9.4.I20090220-1600-e3x" 
  val wikitextile 	= "org.eclipse.mylyn.wikitext" % "wikitext.textile" % "0.9.4.I20090220-1600-e3x" 
  val markdown 		= "org.markdownj" % "markdownj" % "0.3.0-1.0.2b4" 
  val scalatest 	= "org.scalatest" % "scalatest" % "1.0" 
  val scalacheck 	= "org.scala-tools.testing" % "scalacheck" % "1.6" 
  val jmock 		= "org.jmock" % "jmock" % "2.4.0" 
  val easymock 		= "org.easymock" % "easymock" % "2.5.1" 
  val easymockclass	= "org.easymock" % "easymockclassextension" % "2.4" 
  val mockito 		= "org.mockito" % "mockito-all" % "1.8.0" 
  val cglib 		= "cglib" % "cglib" % "2.1_3"  
  val cglibnodep	= "cglib" % "cglib-nodep" % "2.1_3"  
  val objenesis 	= "org.objenesis" % "objenesis" % "1.0"
}