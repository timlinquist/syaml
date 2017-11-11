package org.mulesoft.common.io

import java.io.{FileInputStream, FileOutputStream, InputStreamReader, OutputStreamWriter, File => JFile}

/**
  * Implementation of a SyncFile for the JVM
  */
protected class JvmSyncFile(val fileSystem: FileSystem, val path: String) extends SyncFile {

  private val file = new JFile(path)

  override def async: AsyncFile = new JvmAsyncFile(this)

  override def parent: String      = file.getParent
  override def name: String        = file.getName
  override def list: Array[String] = file.list()

  override def mkdir: Unit = file.mkdirs()

  override def read(encoding: String): CharSequence = {
    val fis  = new InputStreamReader(new FileInputStream(file), encoding)
    val data = new Array[Char](file.length.toInt)
    val n = fis.read(data)
    fis.close()
    if (n == data.length) data else data.subSequence(0, n)
  }

  override def write(data: CharSequence, encoding: String): Unit = {
    val fos = new OutputStreamWriter(new FileOutputStream(file), encoding)
    fos.write(data.toString)
    fos.close()
  }

  override def exists: Boolean      = file.exists()
  override def isDirectory: Boolean = file.isDirectory
  override def isFile: Boolean      = file.isFile
}
