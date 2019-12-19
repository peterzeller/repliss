package crdtver.utils

import java.util.Properties

import scala.util.Using

object ReplissVersion {

  case class Version(version: String, git: String, date: String)

  lazy val version: Version = {
    val resourceName = "versioninfo.properties"
    val loader = Thread.currentThread.getContextClassLoader
    val props = new Properties()
    Using(loader.getResourceAsStream(resourceName)) { resourceStream =>
      props.load(resourceStream)
    }
    Version(
      props.getProperty("version"),
      props.getProperty("gitRevision"),
      props.getProperty("gitDate")
    )
  }

}
