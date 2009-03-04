package org.specs.matcher
import java.io.File

object fileMatchersSpec extends MatchersSpecification with TestFileSystem  {
  
  "The PathMatchers trait" should { doBefore { setFileSystem } 
    "provide an beEqualToIgnoringSep matcher checking if two paths are the same regardless of their separators" in {
      "c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp/hello")
      expectation("c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp2/hello")) must failWith("'c:\\temp\\hello' is not equal ignoring separators to 'c:/temp2/hello'")
    }
    "provide an existPath / beAnExistingPath matcher to check if a file exists" in {
      existingPath must existPath
      existingPath must beAnExistingPath
      expectation(missingPath must beAnExistingPath) must failWith("'absent' doesn't exist")
      expectation(missingPath aka "missing path" must beAnExistingPath) must failWith("missing path 'absent' doesn't exist")
    }
    "provide an beAReadablePath matcher to check if a file can be read" in {
      setReadable(existingPath)
      existingPath must beAReadablePath

      setNotReadable(existingPath)
      expectation(existingPath must beAReadablePath) must failWith("'path' can't be read")
      expectation(existingPath aka "existing path" must beAReadablePath) must failWith("existing path 'path' can't be read")
    }
    "provide an beAWritablePath matcher to check if a file can be written" in {
      setWritable(existingPath)
      existingPath must beAWritablePath

      setNotWritable(existingPath)
      expectation(existingPath must beAWritablePath) must failWith("'path' can't be written")
      expectation(existingPath aka "existing path" must beAWritablePath) must failWith("existing path 'path' can't be written")
    }
    "provide an beAnAbsolutePath matcher to check if a file is absolute" in {
      "/tmp" must beAnAbsolutePath
      expectation(existingPath must beAnAbsolutePath) must failWith("'path' is not absolute")
      expectation(existingPath aka "existing path" must beAnAbsolutePath) must failWith("existing path 'path' is not absolute")
    }
    "provide an beAHiddenPath matcher to check if a file is hidden" in {
      ".tmp" must beAHiddenPath
      expectation(existingPath must beAHiddenPath) must failWith("'path' is not hidden")
      expectation(existingPath aka "existing path" must beAHiddenPath) must failWith("existing path 'path' is not hidden")
    }
    "provide an beAFilePath matcher to check if a file is a file" in {
      "c:/tmp.txt" must beAFilePath
      expectation("tmp/" must beAFilePath) must failWith("'tmp/' is not a file")
      expectation("tmp/" aka "tmp path" must beAFilePath) must failWith("tmp path 'tmp/' is not a file")
    }
    "provide an beADirectoryPath matcher to check if a file is a directory" in {
      "c:/tmp/" must beADirectoryPath
      expectation("test.txt" must beADirectoryPath) must failWith("'test.txt' is not a directory")
      expectation("test.txt" aka "this file" must beADirectoryPath) must failWith("this file 'test.txt' is not a directory")
    }
    "provide an havePathName matcher to check if a file has a given name" in {
      "c:/tmp/test.txt" must havePathName("test.txt")
      expectation("c:/tmp/test.txt" must havePathName("tst.txt")) must failWith("'c:/tmp/test.txt' is not named 'tst.txt'")
      expectation("c:/tmp/test.txt" aka "the file" must havePathName("tst.txt")) must failWith("the file 'c:/tmp/test.txt' is not named 'tst.txt'")
    }
    "provide an haveAsAbsolutePath matcher to check if a file has a given absolute path" in {
      "c:/tmp/test.txt" must haveAsAbsolutePath("c:/tmp/test.txt")
      expectation("c:/tmp/test.txt" must haveAsAbsolutePath("tst.txt")) must failWithMatch("'c:/tmp/test.txt' doesn't have absolute path 'tst.txt' but .*")
      expectation("c:/tmp/test.txt" aka "the file" must haveAsAbsolutePath("tst.txt")) must failWithMatch("the file 'c:/tmp/test.txt' doesn't have absolute path 'tst.txt' but .*")
    }
    "provide an haveAsCanonicalPath matcher to check if a file has a given canonical path" in {
      "c:/tmp/dir/../test.txt" must haveAsCanonicalPath("c:/tmp/test.txt")
      expectation("c:/tmp/dir/test.txt" must haveAsCanonicalPath("c:/tmp/test.txt")) must failWithMatch("'c:/tmp/dir/test.txt' doesn't have canonical path 'c:/tmp/test.txt' but .*")
      expectation("c:/tmp/dir/test.txt" aka "the file" must haveAsCanonicalPath("c:/tmp/test.txt")) must failWithMatch("the file 'c:/tmp/dir/test.txt' doesn't have canonical path 'c:/tmp/test.txt' but .*")
    }
    "provide an haveParentPath matcher to check if a file has a given parent path" in {
      "c:/tmp/dir/test.txt" must haveParentPath("c:/tmp/dir")
      expectation("c:/tmp/dir/test.txt" must haveParentPath("c:/tmp/test.txt")) must failWithMatch("'c:/tmp/dir/test.txt' doesn't have parent path 'c:/tmp/test.txt' but .*")
      expectation("c:/tmp/dir/test.txt" aka "the file" must haveParentPath("c:/tmp/test.txt")) must failWithMatch("the file 'c:/tmp/dir/test.txt' doesn't have parent path 'c:/tmp/test.txt' but .*")
    }
    "provide an listPaths matcher to check if a file has a given children" in {
      addChild("c:/tmp", "c:/tmp/test.txt")
      "c:/tmp" must listPaths("c:/tmp/test.txt")
      expectation("c:/tmp" must listPaths("c:/tmp2/test.txt")) must failWith("'c:/tmp' doesn't have files 'c:/tmp2/test.txt' but 'c:/tmp/test.txt'")
      expectation("c:/tmp" aka "the dir" must listPaths("c:/tmp2/test.txt")) must failWith("the dir 'c:/tmp' doesn't have files 'c:/tmp2/test.txt' but 'c:/tmp/test.txt'")
    }
  }
  "The File matchers" should { doBefore { setFileSystem }
    "provide an exist matcher to check if a file exists" in {
      new File(existingPath) must exist
    }
    "provide an beReadable matcher to check if a file can be read" in {
      setReadable(existingPath)
      new File(existingPath) must beReadable
    }
    "provide an beWritable matcher to check if a file can be written" in {
      setWritable(existingPath)
      new File(existingPath) must beWritable
    }
    "provide an beAbsolute matcher to check if a file is absolute" in {
      new File("/tmp") must beAbsolute
    }
    "provide an beHiddenPath matcher to check if a file is hidden" in {
      new File(".tmp") must beHidden
    }
    "provide an beFile matcher to check if a file is a file" in {
      new File("c:/tmp.txt") must beFile
    }
    "provide an beDirectory matcher to check if a file is a directory" in {
      new File("c:/tmp/") must beDirectory
    }
    "provide an haveName matcher to check if a file has a given name" in {
      new File("c:/tmp/test.txt") must haveName("test.txt")
    }
    "provide an haveAbsolutePath matcher to check if a file has a given absolute path" in {
      new File("c:/tmp/test.txt") must haveAbsolutePath("c:/tmp/test.txt")
    }
    "provide an haveCanonicalPath matcher to check if a file has a given canonical path" in {
      new File("c:/tmp/dir/../test.txt") must haveCanonicalPath("c:/tmp/test.txt")
    }
    "provide an haveParent matcher to check if a file has a given parent path" in {
      new File("c:/tmp/dir/test.txt") must haveParent("c:/tmp/dir")
    }
    "provide an haveList matcher to check if a file has a given children" in {
      addChild("c:/tmp", "c:/tmp/test.txt")
      new File("c:/tmp") must haveList("c:/tmp/test.txt")
    }
  }
  "The File matchers" can { doBefore { setFileSystem } 
    "be used from string using the path function, like 'c:/projects'.path must exist" in {
      existingPath.path must exist
    }
  }
}
import org.specs.io.mock._
trait TestFileSystem extends MockFileSystem {
  val existingPath = "path"
  val missingPath = "absent"
  def setFileSystem  = {  
    reset
    addFile(existingPath, "")
  }
}
import org.specs.runner._
class FileMatchersTest extends JUnit4(fileMatchersSpec)
