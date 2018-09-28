package crdtver.symbolic

import java.io.{File, IOException}
import java.nio.file.{Files, StandardCopyOption}

class Z3Loader {

  private val tmpFolder = new File("./tmp/")

  def loadZ3Library(): Unit = {
    loadLibraryFromJar("libz3.so")
    loadLibraryFromJar("libz3java.so")
    println("library path = " + System.getProperty("java.library.path"))
        addLibraryDir(tmpFolder.getCanonicalPath)
    System.setProperty("java.library.path", tmpFolder.getCanonicalPath)
    val fieldSysPath = classOf[ClassLoader].getDeclaredField("sys_paths")
    fieldSysPath.setAccessible(true)
    fieldSysPath.set(null, null)
    println("library path = " + System.getProperty("java.library.path"))
  }

  private def loadLibraryFromJar(libraryName: String): Unit = {
    val stream = getClass.getClassLoader.getResourceAsStream(libraryName)
    if (stream == null) {
      throw new RuntimeException(s"library $libraryName not found")
    }

    val tempFile = new File(tmpFolder, libraryName)
    if (!tmpFolder.exists()) {
      val r = tmpFolder.mkdirs()
      if (!r) {
        throw new RuntimeException("Could not create directory for " + tempFile)
      }
    }
    Files.copy(stream, tempFile.toPath, StandardCopyOption.REPLACE_EXISTING)
    //    System.load(tempFile.getAbsolutePath)
  }


  def addLibraryDir(s: String): Unit = {
    try { // This enables the java.library.path to be modified at runtime
      // From a Sun engineer at http://forums.sun.com/thread.jspa?threadID=707176
      //
      val field = classOf[ClassLoader].getDeclaredField("usr_paths")
      field.setAccessible(true)
      val paths = field.get(null).asInstanceOf[Array[String]]
      var i = 0
      while (i < paths.length) {
        if (s == paths(i)) {
          return
        }
        i += 1
        i - 1

      }
      val tmp = new Array[String](paths.length + 1)
      System.arraycopy(paths, 0, tmp, 0, paths.length)
      tmp(paths.length) = s
      field.set(null, tmp)
      System.setProperty("java.library.path", System.getProperty("java.library.path") + File.pathSeparator + s)
    } catch {
      case e: IllegalAccessException =>
        throw new IOException("Failed to get permissions to set library path")
      case e: NoSuchFieldException =>
        throw new IOException("Failed to get field handle to set library path")
    }
  }
}
