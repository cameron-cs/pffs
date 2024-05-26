package org.cameron.cs.user

import java.time.Instant

case class Session(user: User, commands: List[String], connectedAt: Instant, disconnectedAt: Option[Instant])
