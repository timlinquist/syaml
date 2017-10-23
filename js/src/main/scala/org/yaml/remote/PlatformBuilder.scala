package org.yaml.remote

/**
  * Platform
  */
object PlatformBuilder extends Platform {

  override val dateTimeOps: PlatFormDateTimeOps = NoneDateTimeOps

  object NoneDateTimeOps extends PlatFormDateTimeOps {
    override def unapply(arg: String): Option[Any] = None
  }
}

