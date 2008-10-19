package org.specs.io

object fileSystemUnit extends TestData {
  "A file system" should {
    "provide a globToPattern function returning the regex pattern corresponding to a glob definition" in {
      paths must pass { matchingPath: MatchingPath => 
        matchingPath.path must beMatching(globToPattern(matchingPath.glob))
      }
    }
  }
}
import scalacheck.Gen._
import scalacheck._
import scala.collection.mutable.Queue
import java.util.regex._
import org.specs._
import org.specs.Sugar._

trait TestData extends Specification with FileSystem with ConsoleOutput with ScalaCheck {
  case class MatchingPath(path: String, glob: String)
  def paths = for { glob <- elements("src/**/*.*", "src/**/hello/**/*.*", "src/test/*.*")
                    path <- elements(pathsMatchingGlob(glob):_*)
                  } yield MatchingPath(path, glob)

  def pathsMatchingGlob(glob: String): List[String] = {
    for { doubleStar   <- List("dir", "dir1/dir2")
          specialChar <-  "!@#$%^&';{}[]".elements.toList 
          name         <- List("name", "name" + specialChar, "name2") 
          ext          <- List("ext1", "ext2") 
        } yield "./" + glob.replace("**", doubleStar).replace(".*", "." + ext).replace("*", name)
  }
}
import org.specs.runner._
class fileSystemUnitTest extends JUnit4(fileSystemUnit)
