package org.cameron.cs

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import org.cameron.cs.file.{Directory, File, FileMetadata, FileSystemEntity}
import org.cameron.cs.ops.{Cd, Copy, CreateDirectory, CreateFile, CreateUser, Delete, FileSystemOp, GrantPermissions, ListDirectory, ListUsers, Move, Pwd, ReadFile, RemoveUser, Rename, SetInitialPermissions, SwitchUser, WhoAmI, WriteFile}
import org.cameron.cs.security.{Execute, Permission, Read, Write}
import org.cameron.cs.user.User

import java.time.Instant
import scala.annotation.tailrec

object FileSystem {

  // Case class representing the state of the file system

  case class FileSystemState(currentDir: Directory, currentUser: User, rootDir: Directory, users: Map[String, User], currentPath: String = "/")

  // Type alias for a state transformation in the context of IO
  type FileSystemStateT[A] = StateT[IO, FileSystemState, A]

  // Default permissions for a directory
  val defaultDirectoryPermissions: Set[Permission] = Set(Read)

  /**
   * Creates a directory at the specified path.
   * Ensures that the parent directory exists before creating the new directory.
   * Updates the state with the new directory.
   * @param path The path where the directory will be created.
   * @return The state transformation representing the creation of the directory.
   */
  def createDirectory(path: String): FileSystemStateT[Unit] =
    ensureParentDirectoryExists(path) *> updateState { state =>
      val pathList = pathToList(path)
      val newDir = Directory(pathList.last, Map.empty, FileMetadata(state.currentUser, Map(state.currentUser.name -> defaultDirectoryPermissions), Instant.now, Instant.now, state.currentUser.name))
      state.copy(rootDir = updateDirectory(
        state.rootDir,
        pathList.dropRight(1),
        self => self.copy(contents = self.contents + (newDir.name -> newDir)))
      )
    }

  /**
   * Grants permissions to a user for a specific path.
   * Only the owner of the entity or the root user can grant permissions.
   * @param path The path of the entity for which permissions are being granted.
   * @param username The username of the user to whom permissions are being granted.
   * @param permissions The set of permissions to be granted.
   * @return The state transformation representing the permission grant.
   */
  def grantPermissions(path: String, username: String, permissions: Set[Permission]): FileSystemStateT[Unit] = updateState { state =>
    val pathList = pathToList(path)
    val entityOpt = findEntity(state.rootDir, pathList)
    if (entityOpt.exists(entity => entity.metadata.owner.name == state.currentUser.name || state.currentUser.name == "root")) {
      val updatedRootDir = updatePermissions(state.rootDir, pathList, permissions, username)
      state.copy(rootDir = updatedRootDir)
    } else {
      state
    }
  }

  /**
   * Updates the permissions of a directory or file.
   * @param dir The root directory.
   * @param path The path of the entity whose permissions are being updated.
   * @param permissions The new set of permissions.
   * @param username The username to whom the permissions are being granted.
   * @return The updated directory.
   */
  private def updatePermissions(dir: Directory, path: List[String], permissions: Set[Permission], username: String): Directory = path match {
    case Nil         => dir
    case head :: Nil =>
      dir.contents.get(head).fold(dir)(entity => dir.copy(contents = dir.contents.updated(head, entity match {
        case file: File => file.copy(metadata = file.metadata.copy(permissions = file.metadata.permissions + (username -> permissions)))
        case subdir: Directory => subdir.copy(metadata = subdir.metadata.copy(permissions = subdir.metadata.permissions + (username -> permissions)))
      })))
    case head :: tail => dir.contents.get(head).fold(dir) {
      case d: Directory => dir.copy(contents = dir.contents.updated(head, updatePermissions(d, tail, permissions, username)))
      case _ => dir
    }
  }

  /**
   * Creates a file in the specified directory with the given properties.
   * Ensures that the parent directory exists before creating the new file.
   * Updates the state with the new file.
   * @param directoryPath The path of the directory where the file will be created.
   * @param name The name of the file.
   * @param content The content of the file as a byte array.
   * @param extension The file extension.
   * @param readable A flag indicating if the file is readable.
   * @return The state transformation representing the creation of the file.
   */
  def createFile(directoryPath: String, name: String, content: Array[Byte], extension: String, readable: Boolean): FileSystemStateT[Unit] =
    ensureParentDirectoryExists(directoryPath) *> updateState { state =>
      val pathList = pathToList(directoryPath)
      val newFile = File(name, content, FileMetadata(state.currentUser, Map(state.currentUser.name -> defaultDirectoryPermissions), Instant.now, Instant.now, state.currentUser.name), extension, readable)
      state.copy(rootDir =
        updateDirectory(
          state.rootDir,
          pathList,
          self => self.copy(contents = self.contents + (newFile.name -> newFile)))
      )
    }

  /**
   * Creates a new user with the given username and password.
   * Only the root user can create new users.
   * Updates the state with the new user.
   * @param name The username of the new user.
   * @param password The password of the new user.
   * @return The state transformation representing the creation of the user.
   */
  def createUser(name: String, password: String): FileSystemStateT[Unit] = updateState { state =>
    if (state.currentUser.name == "root") {
      state.copy(users = state.users + (name -> User(name, password)))
    } else state
  }

  /**
   * Removes an existing user by username.
   * Only the root user can remove users.
   * Updates the state by removing the user.
   * @param username The username of the user to be removed.
   * @return The state transformation representing the removal of the user.
   */
  def removeUser(username: String): FileSystemStateT[Unit] = updateState { state =>
    if (state.currentUser.name == "root") {
      state.copy(users = state.users - username)
    } else state
  }

  /**
   * Returns the current user.
   * @return The current user.
   */
  def whoAmI(): FileSystemStateT[User] = getState.map(_.currentUser)

  /**
   * Reads a file at the specified path.
   * Returns the file if it exists and is readable.
   * @param path The path of the file to be read.
   * @return The state transformation representing the read operation.
   */
  def readFile(path: String): FileSystemStateT[Option[File]] = getState.map { state =>
    checkPermission(state.currentUser, Read, path, state.rootDir)
      .flatMap(_ =>
        findEntity(state.rootDir, pathToList(path))
          .collect {
            case file: File if file.readable => file
          }
      )
  }

  /**
   * Writes content to a file at the specified path.
   * Updates the file's content and metadata.
   * @param path The path of the file to be written to.
   * @param content The content to be written to the file.
   * @param user The user performing the write operation.
   * @return The state transformation representing the write operation.
   */
  def writeFile(path: String, content: Array[Byte], user: User): FileSystemStateT[Unit] = updateState { state =>
    checkPermission(user, Write, path, state.rootDir).fold(state)(_ =>
      state.copy(rootDir = updateEntity(state.rootDir, pathToList(path), {
        case file: File => file.copy(content = content, metadata = file.metadata.copy(lastModified = Instant.now, modifiedBy = user.name))
        case dir: Directory => dir // No update for directories
      }))
    )
  }

  /**
   * Deletes a file or directory at the specified path.
   * Updates the state by removing the entity.
   * @param path The path of the entity to be deleted.
   * @return The state transformation representing the delete operation.
   */
  def delete(path: String): FileSystemStateT[Unit] = updateState { state =>
    checkPermission(state.currentUser, Write, path, state.rootDir).fold(state)(_ =>
      state.copy(rootDir =
        updateDirectory(
          state.rootDir,
          pathToList(path).dropRight(1),
          self => self.copy(contents = self.contents - pathToList(path).last)))
    )
  }

  /**
   * Lists all users.
   * Only the root user can list all users.
   * @return The state transformation representing the list users operation.
   */
  def listUsers(): FileSystemStateT[List[User]] = getState.map { state =>
    if (state.currentUser.name == "root") state.users.values.toList else List.empty[User]
  }

  /**
   * Renames a file or directory.
   * Optionally performs the rename operation recursively.
   * @param srcPath The source path of the entity to be renamed.
   * @param destPath The destination path with the new name.
   * @param recursive A flag indicating if the rename operation should be recursive.
   * @return The state transformation representing the rename operation.
   */
  def rename(srcPath: String, destPath: String, recursive: Boolean): FileSystemStateT[Unit] = for {
    _ <- ensureParentDirectoryExists(destPath)
    _ <- updateState { state =>
      checkPermission(state.currentUser, Write, srcPath, state.rootDir).fold(state) { _ =>
        val srcPathList = pathToList(srcPath)
        val destPathList = pathToList(destPath)
        val entityOpt = findEntity(state.rootDir, srcPathList)
        entityOpt.fold(state) { entity =>
          val parentPath = srcPathList.dropRight(1)
          val newName = destPathList.last
          val updatedDir = entity match {
            case dir: Directory if recursive => updateDirectory(
              state.rootDir,
              parentPath,
              self => self.copy(contents = self.contents - dir.name + (newName -> renameDirectory(dir, newName)))
            )
            case dir: Directory              => updateDirectory(
              state.rootDir,
              parentPath,
              self => self.copy(contents = self.contents - dir.name + (newName -> dir.copy(name = newName)))
            )
            case file: File                  => updateDirectory(
              state.rootDir,
              parentPath,
              self => self.copy(contents = self.contents - file.name + (newName -> file.copy(name = newName))))
          }
          state.copy(rootDir = updatedDir)
        }
      }
    }
  } yield ()

  /**
   * Renames a directory and its contents recursively.
   * @param dir The directory to be renamed.
   * @param newName The new name for the directory.
   * @return The updated directory.
   */
  private def renameDirectory(dir: Directory, newName: String): Directory = {
    val updatedContents = dir.contents.map {
      case (name, entity) => entity match {
        case subDir: Directory => newName -> renameDirectory(subDir, name)
        case file: File => name -> file.copy(name = newName)
      }
    }
    dir.copy(name = newName, contents = updatedContents)
  }

  /**
   * Lists the contents of a directory.
   * @param path The path of the directory.
   * @return The state transformation representing the list directory operation.
   */
  def listDirectory(path: String): FileSystemStateT[List[FileSystemEntity]] = getState.map { state =>
    checkPermission(state.currentUser, Read, path, state.rootDir).flatMap(_ => findEntity(state.rootDir, pathToList(path)).collect { case dir: Directory => dir.contents.values.toList }).getOrElse(Nil)
  }

  /**
   * Returns the current working directory.
   * @return The current working directory as a string.
   */
  def pwd(): FileSystemStateT[String] = getState.map(_.currentPath)

  /**
   * Copies a file or directory to a new location.
   * @param srcPath The source path of the entity to be copied.
   * @param destPath The destination path where the entity will be copied.
   * @param user The user performing the copy operation.
   * @return The state transformation representing the copy operation.
   */
  def copy(srcPath: String, destPath: String, user: User): FileSystemStateT[Unit] = for {
    entity <- getState.map { state =>
      checkPermission(user, Read, srcPath, state.rootDir).flatMap(_ => findEntity(state.rootDir, pathToList(srcPath)))
    }
    _ <- entity match {
      case Some(file: File) => createFile(pathToList(destPath).dropRight(1).mkString("/"), pathToList(destPath).last, file.content, file.extension, file.readable)
      case Some(directory: Directory) => copyDirectory(directory, pathToList(destPath), user)
      case None => StateT.pure[IO, FileSystemState, Unit](())
    }
  } yield ()

  /**
   * Copies a directory and its contents to a new location.
   * @param directory The directory to be copied.
   * @param destPath The destination path where the directory will be copied.
   * @param user The user performing the copy operation.
   * @return The state transformation representing the copy directory operation.
   */
  def copyDirectory(directory: Directory, destPath: List[String], user: User): FileSystemStateT[Unit] = for {
    _ <- createDirectory(destPath.mkString("/"))
    _ <- directory.contents.toList.traverse {
      case (name, file: File) => createFile(destPath.mkString("/"), name, file.content, file.extension, file.readable)
      case (name, subDir: Directory) => copyDirectory(subDir, destPath :+ name, user)
    }
  } yield ()

  /**
   * Moves a file or directory to a new location.
   * @param srcPath The source path of the entity to be moved.
   * @param destPath The destination path where the entity will be moved.
   * @param user The user performing the move operation.
   * @return The state transformation representing the move operation.
   */
  def move(srcPath: String, destPath: String, user: User): FileSystemStateT[Unit] =
    copy(srcPath, destPath, user) *> delete(srcPath)

  /**
   * Ensures that the parent directory exists for a given path.
   * Creates the parent directory if it does not exist.
   * @param path The path for which the parent directory should be ensured.
   * @return The state transformation representing the ensure parent directory exists operation.
   */
  private def ensureParentDirectoryExists(path: String): FileSystemStateT[Unit] = {
    val parentPath = pathToList(path).dropRight(1)
    parentPath.foldLeft(StateT.pure[IO, FileSystemState, Unit](())) { (state, dir) =>
      state.flatMap(_ => updateState { fsState =>
        if (findEntity(fsState.rootDir, pathToList(fsState.rootDir.name) ++ List(dir)).isEmpty) {
          val newDir = Directory(dir, Map.empty, FileMetadata(fsState.currentUser, Map(fsState.currentUser.name -> Set(Read, Write, Execute)), Instant.now, Instant.now, fsState.currentUser.name))
          fsState.copy(rootDir =
            updateDirectory(
              fsState.rootDir,
              pathToList(newDir.name).dropRight(1),
              self => self.copy(contents = self.contents + (newDir.name -> newDir))))
        } else fsState
      })
    }
  }

  /**
   * Checks if a user has a specific permission for a file system entity.
   * @param username The username of the user.
   * @param fsEntity The file system entity.
   * @param permission The permission to be checked.
   * @return True if the user has the permission, false otherwise.
   */
  private def hasPermission(username: String, fsEntity: FileSystemEntity, permission: Permission): Boolean =
    fsEntity.metadata.owner.name == username || fsEntity.metadata.permissions.get(username).exists(_.contains(permission))

  /**
   * Changes the current working directory to the specified path.
   * @param path The path of the new working directory.
   * @return The state transformation representing the change directory operation.
   */
  def cd(path: String): FileSystemStateT[Unit] = updateState { state =>
    val newPath = if (path.startsWith("/")) pathToList(path) else pathToList(state.currentPath) ++ pathToList(path)
    findEntity(state.rootDir, newPath).collect {
      case dir: Directory if hasPermission(state.currentUser.name, dir, Read) =>
        state.copy(currentDir = dir, currentPath = newPath.mkString("/", "/", ""))
    }.getOrElse(state)
  }

  /**
   * Switches the current user to a new user.
   * @param user The new user.
   * @return The state transformation representing the switch user operation.
   */
  def switchUser(user: User): FileSystemStateT[Unit] = updateState(_.copy(currentUser = user))

  /**
   * Checks if a user has a specific permission for a path.
   * @param user The user.
   * @param permission The permission to be checked.
   * @param path The path of the entity.
   * @param dir The root directory.
   * @return An option containing Unit if the user has the permission, None otherwise.
   */
  private def checkPermission(user: User, permission: Permission, path: String, dir: Directory): Option[Unit] =
    findEntityWithPermissionCheck(user, permission, pathToList(path), dir)

  /**
   * Recursively finds an entity and checks if the user has the required permission.
   * @param user The user.
   * @param permission The permission to be checked.
   * @param path The path of the entity.
   * @param dir The root directory.
   * @return An option containing Unit if the user has the permission, None otherwise.
   */
  @tailrec
  private def findEntityWithPermissionCheck(user: User, permission: Permission, path: List[String], dir: Directory): Option[Unit] = path match {
    case Nil => Some(())
    case head :: tail =>
      dir.contents.get(head) match {
        case Some(subDir: Directory) if hasPermission(user.name, subDir, permission)        =>
          findEntityWithPermissionCheck(user, permission, tail, subDir)
        case Some(file: File) if tail.isEmpty && hasPermission(user.name, file, permission) =>
          Some(())
        case _                                                                              =>
          None
      }
  }

  /**
   * Finds an entity by its path.
   * @param dir The root directory.
   * @param path The path of the entity.
   * @return An option containing the entity if found, None otherwise.
   */
  private def findEntity(dir: Directory, path: List[String]): Option[FileSystemEntity] = path match {
    case Nil => Some(dir)
    case head :: tail => dir.contents.get(head).flatMap {
      case subDir: Directory => findEntity(subDir, tail)
      case file: File if tail.isEmpty => Some(file)
      case _ => None
    }
  }

  /**
   * Updates an entity in the directory.
   * @param dir The root directory.
   * @param path The path of the entity to be updated.
   * @param f The function to update the entity.
   * @return The updated directory.
   */
  private def updateEntity(dir: Directory, path: List[String], f: FileSystemEntity => FileSystemEntity): Directory = path match {
    case Nil            => dir
    case head :: Nil    => dir.contents.get(head).fold(dir)(entity => dir.copy(contents = dir.contents.updated(head, f(entity))))
    case head :: tail   => dir.contents.get(head).fold(dir) {
      case d: Directory => dir.copy(contents = dir.contents.updated(head, updateEntity(d, tail, f)))
      case _ => dir
    }
  }

  /**
   * Updates a directory.
   * @param dir The root directory.
   * @param path The path of the directory to be updated.
   * @param f The function to update the directory.
   * @return The updated directory.
   */
  private def updateDirectory(dir: Directory, path: List[String], f: Directory => Directory): Directory = path match {
    case Nil                 => f(dir)
    case head :: tail        => dir.contents.get(head).fold(dir.copy(contents = dir.contents + (head -> updateDirectory(Directory(head, Map.empty, dir.metadata), tail, f)))) {
      case subDir: Directory => dir.copy(contents = dir.contents.updated(head, updateDirectory(subDir, tail, f)))
      case _                 => dir
    }
  }

  /**
   * Converts a path to a list of strings.
   * @param path The path to be converted.
   * @return The list of strings representing the path.
   */
  def pathToList(path: String): List[String] =
    path.split("/").filter(_.nonEmpty).toList

  /**
   * Interprets a file system operation.
   * @param op The file system operation to be interpreted.
   * @return The state transformation representing the interpretation of the operation.
   */
  def interpret[A](op: FileSystemOp[A]): FileSystemStateT[A] = op match {
    case CreateFile(path, name, content, extension, readable) => createFile(path, name, content, extension, readable)
    case CreateDirectory(path) => createDirectory(path)
    case ReadFile(path) => readFile(path)
    case WriteFile(path, content, user) => writeFile(path, content, user)
    case Delete(path) => delete(path)
    case ListDirectory(path) => listDirectory(path)
    case Pwd() => pwd()
    case Copy(srcPath, destPath, user) => copy(srcPath, destPath, user)
    case Move(srcPath, destPath, user) => move(srcPath, destPath, user)
    case SwitchUser(user) => switchUser(user)
    case SetInitialPermissions() => setInitialPermissions()
    case Rename(srcPath, destPath, recursive) => rename(srcPath, destPath, recursive)
    case CreateUser(name, password) => createUser(name, password)
    case GrantPermissions(path, username, permissions) => grantPermissions(path, username, permissions)
    case Cd(path) => cd(path)
    case ListUsers => listUsers()
    case RemoveUser(username) => removeUser(username)
    case WhoAmI               => whoAmI()
  }

  /**
   * Sets initial permissions for the root directory.
   * @return The state transformation representing the set initial permissions operation.
   */
  private def setInitialPermissions(): FileSystemStateT[Unit] =
    updateState { state =>
      state.copy(rootDir = state.rootDir.copy(metadata = state.rootDir.metadata.copy(permissions = Map(state.currentUser.name -> Set(Read, Write, Execute)))))
    }

  /**
   * Updates the state by applying the given function.
   * @param f The function to update the state.
   * @return The state transformation representing the update state operation.
   */
  private def updateState(f: FileSystemState => FileSystemState): FileSystemStateT[Unit] =
    StateT.modifyF[IO, FileSystemState](state => IO(f(state)))

  /**
   * Gets the current state.
   * @return The current state.
   */
  private def getState: FileSystemStateT[FileSystemState] =
    StateT.get[IO, FileSystemState]
}