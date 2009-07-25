import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = Unchecked :: super.compileOptions.toList
  override def javaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m -Xss1M") :: Nil
  override def testJavaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m") :: Nil
  val junit 		= "junit" % "junit" % "4.5"
  val wikitext 		= "org.eclipse.mylyn.wikitext" % "wikitext" % "0.9.4.I20090220-1600-e3x" 
  val wikitextile 	= "org.eclipse.mylyn.wikitext" % "wikitext.textile" % "0.9.4.I20090220-1600-e3x" 
  val markdown 		= "org.markdownj" % "markdownj" % "0.3.0-1.0.2b4" 
  val scalatest 	= "org.scala-tools.testing" % "scalatest" % "0.9.5" 
  val scalacheck 	= "org.scalacheck" % "scalacheck" % "1.5" 
  val jmock 		= "org.jmock" % "jmock" % "2.4.0" 
  val mockito 		= "org.mockito" % "mockito-all" % "1.7" 
  val cglib 		= "cglib" % "cglib" % "2.1_3"  
  val objenesis 	= "org.objenesis" % "objenesis" % "1.0"
}