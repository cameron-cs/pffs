package org.cameron.cs.file

import org.cameron.cs.security.Permission
import org.cameron.cs.user.User

import java.time.Instant

case class FileMetadata(owner: User, permissions: Map[String, Set[Permission]], created: Instant, lastModified: Instant, modifiedBy: String, size: Long)