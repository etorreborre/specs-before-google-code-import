name := "specs"

organization := "org.scala-tools.testing"

version := "1.6.9"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.7" % "optional",
  "org.eclipse.mylyn.wikitext" % "wikitext" % "0.9.4.I20090220-1600-e3x" % "optional",
  "org.eclipse.mylyn.wikitext" % "wikitext.textile" % "0.9.4.I20090220-1600-e3x"% "optional",
  "org.markdownj" % "markdownj" % "0.3.0-1.0.2b4" % "optional",
  "org.scala-tools.testing" % "test-interface" % "0.5" % "optional",
  "org.scalatest" % "scalatest" % "1.2" % "optional",
  "org.easymock" % "easymock" % "2.5.1" % "optional",
  "org.easymock" % "easymockclassextension" % "2.4" % "optional",
  "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "optional",
  "org.jmock" % "jmock" % "2.5.1" % "optional",
  "org.jmock" % "jmock-legacy" % "2.5.1" % "optional",
  "org.mockito" % "mockito-all" % "1.8.5" % "optional",
  "cglib" % "cglib" % "2.1_3" % "optional",
  "org.objenesis" % "objenesis" % "1.0" % "optional",
  "org.scala-lang" % "scala-compiler" % "2.9.2" % "optional")

testOptions := Seq(Tests.Filter(s => s.endsWith("Spec")))

resolvers ++= Seq("Sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

/** Publishing */
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some("sonatype-snapshots" at nexus + "content/repositories/snapshots")
  else                             Some("sonatype-staging" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://code.google.com/p/specs/</url>
  <licenses>
    <license>
      <name>MIT-style</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>http://github.com/etorreborre/specs</url>
    <connection>scm:http:http://etorreborre@github.com/etorreborre/specs.git</connection>
  </scm>
  <developers>
    <developer>
      <id>etorreborre</id>
      <name>Eric Torreborre</name>
      <url>http://etorreborre.blogspot.com/</url>
      </developer>
    </developers>
)