# Purely Functional File System Project (pffs)

## Overview
This project implements a simple in-memory file system using functional programming principles in Scala. The file system supports basic operations such as creating directories and files, reading and writing files, changing directories, and managing users and permissions. The state of the file system is managed using cats.data.StateT in the context of cats.effect.IO.

## File System State

The file system maintains its state in the FileSystemState case class, which includes:

- currentDir: The current directory.

- currentUser: The currently logged-in user.

- rootDir: The root directory of the file system.

- users: A map of usernames to user objects.

- currentPath: The current path as a string.

### Operations

#### Directory operations

- createDirectory(path: String): Creates a directory at the specified path.

- listDirectory(path: String): Lists the contents of a directory.

- cd(path: String): Changes the current working directory to the specified path.

- rename(srcPath: String, destPath: String, recursive: Boolean): Renames a file or directory.

#### File operations

- createFile(directoryPath: String, name: String, content: Array[Byte], extension: String, readable: Boolean): Creates a file in the specified directory.

- readFile(path: String): Reads a file at the specified path.

- writeFile(path: String, content: Array[Byte], user: User): Writes content to a file.

- delete(path: String): Deletes a file or directory at the specified path.

- copy(srcPath: String, destPath: String, user: User): Copies a file or directory to a new location.

- move(srcPath: String, destPath: String, user: User): Moves a file or directory to a new location.

#### User operations

- createUser(name: String, password: String): Creates a new user with the given username and password.

- removeUser(username: String): Removes an existing user by username.

- switchUser(user: User): Switches the current user to a new user.

- whoAmI(): Returns the current user.

- listUsers(): Lists all users. Only the root user can list all users.

- grantPermissions(path: String, username: String, permissions: Set[Permission]): Grants permissions to a user for a specific path.


### Internal Helpers

#### Permission checks
 
- hasPermission(username: String, fsEntity: FileSystemEntity, permission: Permission): Checks if a user has a specific permission for a file system entity.

- checkPermission(user: User, permission: Permission, path: String, dir: Directory): Checks if a user has a specific permission for a path.

#### Entity management

- findEntity(dir: Directory, path: List[String]): Finds an entity by its path.

- updateEntity(dir: Directory, path: List[String], f: FileSystemEntity => FileSystemEntity): Updates an entity in the directory.

- updateDirectory(dir: Directory, path: List[String], f: Directory => Directory): Updates a directory.

#### Path management
- pathToList(path: String): Converts a path to a list of strings.

### Example usage

```scala
val initialRootUser = User("root", "rootpassword")
val initialRoot = Directory("/", Map.empty, FileMetadata(initialRootUser, Map(initialRootUser.name -> Set(Read, Write, Execute)), Instant.now, Instant.now, initialRootUser.name, 0))
val initialState = FileSystemState(initialRoot, initialRootUser, initialRoot, Map("root" -> initialRootUser))

val program = for {
_ <- FileSystem.interpret(CreateDirectory("/workspace"))
_ <- FileSystem.interpret(Cd("/workspace"))
pwd <- FileSystem.interpret(Pwd)
} yield pwd

runProgram(program).unsafeRunSync() // Should print: /workspace
```

Overview of the main features and usage of the in-memory file system implemented in Scala using functional programming principles. The project demonstrates how to manage state and handle file system operations in a purely functional way.