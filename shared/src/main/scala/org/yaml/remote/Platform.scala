package org.yaml.remote

/**
  * Platform
  */
trait Platform {
  val dateTimeOps: PlatFormDateTimeOps
}

trait PlatFormDateTimeOps {
  def unapply(arg: String): Option[Any]
}

object PlatformOps {
  val platform: Platform = PlatformBuilder
}
