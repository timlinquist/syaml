package org.mulesoft.common.io

import scala.scalajs.js

/**
  * Stats
  */
@js.native
trait Stats extends js.Object {
  def dev: Int            = js.native
  def ino: Double         = js.native
  def mode: Integer       = js.native
  def nlink: Int          = js.native
  def uid: Int            = js.native
  def gid: Int            = js.native
  def rdev: Int           = js.native
  def size: Double        = js.native
  def blksize: Int        = js.native
  def blocks: Int         = js.native
  def atime: js.Date      = js.native
  def atimeMs: Double     = js.native
  def mtime: js.Date      = js.native
  def mtimeMs: Double     = js.native
  def ctime: js.Date      = js.native
  def ctimeMs: Double     = js.native
  def birthtime: js.Date  = js.native
  def birthtimeMs: Double = js.native

  def isFile(): Boolean            = js.native
  def isDirectory(): Boolean       = js.native
  def isBlockDevice(): Boolean     = js.native
  def isCharacterDevice(): Boolean = js.native
  def isSymbolicLink(): Boolean    = js.native
  def isFIFO(): Boolean            = js.native
  def isSocket(): Boolean          = js.native
}
