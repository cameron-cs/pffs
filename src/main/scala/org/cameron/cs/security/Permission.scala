package org.cameron.cs.security

sealed trait Permission
case object Read extends Permission
case object Write extends Permission
case object Execute extends Permission