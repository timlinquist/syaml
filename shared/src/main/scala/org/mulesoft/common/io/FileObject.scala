package org.mulesoft.common.io

import scala.concurrent.Future
import scala.language.higherKinds

/**
  * A File Object abstraction (Similar to java.util.File) but with implementations in Js and JVM.
  */
protected[io] trait File[F[_]] {

  /** Returns an async view of the file */
  def async: AsyncFile

  /** Returns a sync view of the file */
  def sync: SyncFile

  /** list the contents of a directory. */
  def list: F[Array[String]]

  /** Create a directory. */
  def mkdir: F[Unit]

  /** Read the file. */
  def read(encoding: String = Utf8): F[CharSequence]

  /** Write to the file. */
  def write(data: CharSequence, encoding: String = Utf8): F[Unit]

  /** Returns the Filesystem for this file */
  def fileSystem: FileSystem

  /**
    * The whole file path
    */
  def path: String

  /**
    * Returns the pathname string of this abstract pathname's parent, or empty
    * if this pathname does not name a parent directory.
    */
  def parent: String

  /**
    * Returns the name of the file or directory denoted by this abstract
    * pathname.  This is just the last name in the pathname's name sequence.
    */
  def name: String

  /** Returns true if the File exists */
  def exists: F[Boolean]

  /** Returns true if the File is a directory */
  def isDirectory: F[Boolean]

  /** Returns true if the File is a normal Filey */
  def isFile: F[Boolean]

  override def toString: String = path
}

trait AsyncFile extends File[Future] {
  override def async: AsyncFile = this
}

trait SyncFile extends File[Id] {
  override def sync: SyncFile = this
}
