package org.specs.matcher
import java.io.File
import org.specs.matcher.MatcherUtils.{q, matches}
import org.specs.io.{FileSystem, ConsoleOutput}

/**
 * The <code>PathMatchers</code> trait provides matchers which are applicable to strings representing paths
 */
trait PathMatchers extends FileSystem {
  
  /**
   * Matches if new File(path).exists
   */   
  def beAnExistingPath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && exists(path), d(path) + " exists", d(path) + " doesn't exist")} 
  } 
  /** alias for beAnExistingFile */
  def existPath[T <: String] = beAnExistingPath[T] 

  /**
   * Matches if new File(path).canRead
   */   
  def beAReadablePath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && canRead(path), d(path) + " is readable", d(path) + " can't be read")} 
  } 

  /**
   * Matches if new File(path).canWrite
   */   
  def beAWritablePath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && canWrite(path), d(path) + " is writable", d(path) + " can't be written")} 
  } 

  /**
   * Matches if new File(path).isAbsolute
   */   
  def beAnAbsolutePath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && isAbsolute(path), d(path) + " is absolute", d(path) + " is not absolute")} 
  } 

  /**
   * Matches if new File(path).isHidden
   */   
  def beAHiddenPath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && isHidden(path), d(path) + " is hidden", d(path) + " is not hidden")} 
  } 

  /**
   * Matches if new File(path).isFile
   */   
  def beAFilePath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && isFile(path), d(path) + " is a file", d(path) + " is not a file")} 
  } 

  /**
   * Matches if new File(path).isDirectory
   */   
  def beADirectoryPath[T <: String] = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && isDirectory(path), d(path) + " is a directory", d(path) + " is not a directory")} 
  } 

  /**
   * Matches if new File(path).getName == name
   */   
  def havePathName[T <: String](name: String) = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (isEqualIgnoringSep(getName(path), name), d(path) + " is named " + q(name), d(path) + " is not named " + q(name))} 
  } 
  /**
   * Matches if new File(path).getAbsolutePath == absolutePath
   */   
  def haveAsAbsolutePath[T <: String](absolutePath: String) = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (isEqualIgnoringSep(path, absolutePath), d(path) + " has absolute path " + q(absolutePath), d(path) + " doesn't have absolute path " + q(absolutePath) + " but " + q(getAbsolutePath(path)))} 
  } 
  /**
   * Matches if new File(path).getCanonicalPath == canonicalPath
   */   
  def haveAsCanonicalPath[T <: String](canonicalPath: String) = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (isEqualIgnoringSep(getCanonicalPath(path), canonicalPath), d(path) + " has canonical path " + q(canonicalPath), d(path) + " doesn't have canonical path " + q(canonicalPath) + " but " + q(getCanonicalPath(path)))} 
  } 
  /**
   * Matches if new File(path).getParent == parent
   */   
  def haveParentPath[T <: String](parent: String) = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (isEqualIgnoringSep(getParent(path), parent), d(path) + " has parent path " + q(parent), d(path) + " doesn't have parent path " + q(parent) + " but " + q(getParent(path)))} 
  } 
  /**
   * Matches if new File(path).list == list(files)
   */   
  def listPaths[T <: String](list: String*) = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (path != null && list != null && listFiles(path).toList == list.toList, 
                                         d(path) + " has files " + q(list.mkString(", ")), 
                                         d(path) + " doesn't have files " + q(list.toList.mkString(", ")) + " but " + q(listFiles(path).toList.mkString(", ")))} 
  } 
  /**
   * Matches if 2 paths are the same regardless of their separators
   * @deprecated use beEqualToIgnoringSep instead
   */   
  def beEqualIgnoringSep[T <: String](other: String) = beEqualToIgnoringSep(other)
  /**
   * Matches if 2 paths are the same regardless of their separators
   */   
  def beEqualToIgnoringSep[T <: String](other: String) = new Matcher[T](){ 
    def apply(v: => T) = {val path = v; (isEqualIgnoringSep(path, other) , d(path) + " is equal ignoring separators to " + q(other), d(path) + " is not equal ignoring separators to " + q(other))} 
  }
  /** @return true if the 2 paths are equal, ignoring separators */
  def isEqualIgnoringSep[T <: String](path: T, other: String) = path != null && other != null&& getCanonicalPath(path).replaceAll("\\\\", "/") == getCanonicalPath(other).replaceAll("\\\\", "/") 
  
}
/**
 * The <code>FileMatchers</code> trait provides matchers which are applicable to files
 */
trait FileMatchers extends PathMatchers {
  /**
   * Matches if file.exists
   */   
  def exist[T <: File] = (existPath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.canRead
   */   
  def beReadable[T <: File] = (beAReadablePath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.canWrite
   */   
  def beWritable[T <: File] = (beAWritablePath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.isAbsolute
   */   
  def beAbsolute[T <: File] = (beAnAbsolutePath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.isHidden
   */   
  def beHidden[T <: File] = (beAHiddenPath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.isFile
   */   
  def beFile[T <: File] = (beAFilePath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.isDirectory
   */   
  def beDirectory[T <: File] = (beADirectoryPath[String]) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.getName == name
   */   
  def haveName[T <: File](name: String) = (havePathName(name)) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.getAbsolutePath == path
   */   
  def haveAbsolutePath[T <: File](path: String) = (haveAsAbsolutePath(path)) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.getCanonicalPath == path
   */   
  def haveCanonicalPath[T <: File](path: String) = (haveAsCanonicalPath(path)) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.getParent == path
   */   
  def haveParent[T <: File](path: String) = (haveParentPath(path)) ^^ ((f: T) => f.getPath)

  /**
   * Matches if file.list == list
   */   
  def haveList[T <: File](list: String) = (listPaths(list)) ^^ ((f: T) => f.getPath)
}
