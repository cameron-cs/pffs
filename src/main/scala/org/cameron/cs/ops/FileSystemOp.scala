package org.cameron.cs.ops

import org.cameron.cs.file.{File, FileSystemEntity}
import org.cameron.cs.security.Permission
import org.cameron.cs.user.{Session, User}

/**
 * Sealed trait representing various file system operations.
 */
sealed trait FileSystemOp[A]

/**
 * Operation to create a file.
 * @param path The directory path where the file will be created.
 * @param name The name of the file.
 * @param extension The file extension.
 * @param readable A flag indicating if the file is readable.
 */
case class CreateFile(path: String, name: String, extension: String, readable: Boolean) extends FileSystemOp[Unit]

/**
 * Operation to write a file content.
 * @param path The directory path where the file will be created.
 * @param name The name of the file.
 * @param content The content of the file as a byte array.
 */
case class WriteFileContent(path: String, name: String, content: Array[Byte]) extends FileSystemOp[Unit] {
  override def toString: String = s"WriteFileContent($path,$name)"
}

/**
 * Operation to list all users. This is accessible only to the root user.
 */
case object ListUsers extends FileSystemOp[List[User]]

/**
 * Operation to create a directory.
 * @param path The path where the directory will be created.
 */
case class CreateDirectory(path: String) extends FileSystemOp[Unit]

/**
 * Operation to create a new user.
 * @param name The username.
 * @param password The password for the new user.
 */
case class CreateUser(name: String, password: String) extends FileSystemOp[Unit]

/**
 * Operation to remove an existing user.
 * @param username The username of the user to be removed.
 */
case class RemoveUser(username: String) extends FileSystemOp[Unit]

/**
 * Operation to grant permissions to a user for a specific path.
 * @param path The path for which permissions are being granted.
 * @param username The username of the user being granted permissions.
 * @param permissions The set of permissions to be granted.
 */
case class GrantPermissions(path: String, username: String, permissions: Set[Permission]) extends FileSystemOp[Unit]

/**
 * Operation to read a file.
 * @param path The path of the file to be read.
 */
case class ReadFile(path: String) extends FileSystemOp[Option[File]]

/**
 * Operation to write content to a file.
 * @param path The path of the file to be written to.
 * @param content The content to be written as a byte array.
 * @param user The user performing the write operation.
 */
case class WriteFile(path: String, content: Array[Byte], user: User) extends FileSystemOp[Unit]

/**
 * Operation to remove a file or directory.
 * @param path The path of the file or directory to be deleted.
 */
case class Remove(path: String) extends FileSystemOp[Unit]

/**
 * Operation to list the contents of a directory.
 * @param path The path of the directory to be listed.
 */
case class ListDirectory(path: String) extends FileSystemOp[List[FileSystemEntity]]

/**
 * Operation to get the current working directory.
 */
case object Pwd extends FileSystemOp[String]

/**
 * Operation to copy a file or directory to a new location.
 * @param srcPath The source path of the file or directory to be copied.
 * @param destPath The destination path where the file or directory will be copied.
 * @param user The user performing the copy operation.
 */
case class Copy(srcPath: String, destPath: String, user: User) extends FileSystemOp[Unit]

/**
 * Operation to move a file or directory to a new location.
 * @param srcPath The source path of the file or directory to be moved.
 * @param destPath The destination path where the file or directory will be moved.
 * @param user The user performing the move operation.
 */
case class Move(srcPath: String, destPath: String, user: User) extends FileSystemOp[Unit]

/**
 * Operation to rename a file or directory.
 * @param srcPath The source path of the file or directory to be renamed.
 * @param destPath The new name or path for the file or directory.
 * @param recursive A flag indicating if the renaming should be recursive for directories.
 */
case class Rename(srcPath: String, destPath: String, recursive: Boolean = false) extends FileSystemOp[Unit]

/**
 * Operation to switch the current user.
 * @param user The user to switch to.
 */
case class SwitchUser(user: User) extends FileSystemOp[Unit]

/**
 * Operation to set initial permissions for the root directory.
 */
case class SetInitialPermissions() extends FileSystemOp[Unit]

/**
 * Operation to change the current working directory.
 * @param path The path to change to.
 */
case class Cd(path: String) extends FileSystemOp[Unit]

/**
 * Operation to get the current user.
 */
case object WhoAmI extends FileSystemOp[User]

/**
 * Operation to get the tree of the dir.
 */
case class Tree(path: String) extends FileSystemOp[String]

/**
 * Operation to end the current user's session.
 */
case object Exit extends FileSystemOp[Unit]

/**
 * Operation to get all the user sessions.
 */
case object GetSessions extends FileSystemOp[List[Session]]

/**
 * Operation to get the current user's session.
 */
case object GetJournal extends FileSystemOp[List[String]]

/**
 * Operation to open the file.
 */
case class OpenFile(path: String) extends FileSystemOp[Int]

/**
 * Operation to close the file.
 */
case class CloseFile(fd: Int) extends FileSystemOp[Unit]

/**
 * Operation to read the file by file descriptor.
 */
case class ReadFileByFd(fd: Int) extends FileSystemOp[Option[Array[Byte]]]

/**
 * Operation to write the content to the file by file descriptor.
 */
case class WriteFileByFd(fd: Int, content: Array[Byte]) extends FileSystemOp[Unit]
