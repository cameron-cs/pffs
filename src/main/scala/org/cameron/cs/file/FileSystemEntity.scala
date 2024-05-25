package org.cameron.cs.file

sealed trait FileSystemEntity {
  def name: String
  def metadata: FileMetadata
}

case class File(name: String, content: Array[Byte], metadata: FileMetadata, extension: String, readable: Boolean) extends FileSystemEntity

case class Directory(name: String, contents: Map[String, FileSystemEntity], metadata: FileMetadata) extends FileSystemEntity