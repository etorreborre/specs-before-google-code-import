package org.specs.io
import org.specs._
import org.specs.runner._

class fileSystemTest extends JUnit4(fileSystemSpec)
object fileSystemSpec extends Specification {
  
  "A FileSystem" should { 
    "list all files in a directory with filePaths()" in {
      fs.filePaths("./src/test/scala/org/specs/io") mustExistMatch "fileSystemSpec"      
    }
    "list all files in a directory assuming local directory if no point starts the path" in {
      fs.filePaths("src/test/scala/org/specs/io") mustExistMatch "fileSystemSpec"      
    }
    "not list directories in a directory with filePaths()" in {
      fs.filePaths("./src/test/scala/org/specs") mustNotExistMatch "io$"      
    }
    "list all files in a directory with filePaths() using a glob pattern" in {
      fs.filePaths("./src/test/scala/org/specs/io/*.*") mustExistMatch "fileSystemSpec"      
      fs.filePaths("./src/test/scala/org/specs/**/*.*") mustExistMatch "fileSystemSpec"      
    }
    "list file paths using a glob pattern like /dir/**/dir2/*.*" in {
      fs.filePaths("./**/io/*.*") mustExistMatch "fileSystemSpec"      
    }
    "list file paths using /**/*name.ext and return only relevant files" in {
      fs.filePaths("./**/io/*mSpec.*") mustNotExistMatch "fileWriterSpec"      
    }
    doAfter { fs.removeDir("./testingDir") }
    "remove a directory and all its content recursively with removeDir" in {
      fs.createDir("./testingDir/directoryToRemove")
      fs.createFile("./testingDir/directoryToRemove/testFile.txt")
      fs.removeDir("./testingDir")
      fs.exists("./testingDir") must beFalse    
    }
    "remove a directory with removeDir and return the parent path" in {
      fs.createDir("./testingDir/directoryToRemove")
      fs.createFile("./testingDir/directoryToRemove/testFile.txt")
      fs.removeDir("./testingDir") must_== "."      
    }
    "not remove a file with the removeDir method" in {
      fs.createDir("./testingDir/directoryToRemove")
      fs.createFile("./testingDir/directoryToRemove/testFile.txt")
      fs.removeDir("./testingDir/directoryToRemove/testFile.txt")
      fs.filePaths("./testingDir/directoryToRemove") mustExistMatch "testFile"      
    }
  }
}
