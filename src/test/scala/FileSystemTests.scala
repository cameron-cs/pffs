import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import org.cameron.cs.FileSystem
import org.cameron.cs.FileSystem.{FileSystemState, FileSystemStateT, findEntity}
import org.cameron.cs.file.{Directory, File, FileMetadata}
import org.cameron.cs.ops.{Cd, CreateDirectory, CreateFile, CreateUser, Delete, GrantPermissions, ListUsers, Move, Pwd, ReadFile, Rename, SwitchUser, WhoAmI}
import org.cameron.cs.security.{Execute, Read, Write}
import org.cameron.cs.user.User
import org.scalatest.funsuite.AsyncFunSuite

import java.time.Instant

class FileSystemTests extends AsyncFunSuite with AsyncIOSpec {

  val initialRootUser = User("root", "rootpassword")
  val initialRoot = Directory("/", Map.empty, FileMetadata(initialRootUser, Map(initialRootUser.name -> Set(Read, Write, Execute)), Instant.now, Instant.now, initialRootUser.name))
  val initialState = FileSystemState(initialRoot, initialRootUser, initialRoot, Map("root" -> initialRootUser))

  def runProgram[A](program: FileSystemStateT[A]): IO[A] = {
    program.runA(initialState)
  }

  test("create and read a file") {
    val program = for {
      _ <- FileSystem.interpret(CreateDirectory("/docs"))
      _ <- FileSystem.interpret(CreateFile("/docs", "file1.txt", "Hello, World!".getBytes("UTF-8"), ".txt", true))
      file <- FileSystem.interpret(ReadFile("/docs/file1.txt"))
    } yield file

    runProgram(program).asserting { file =>
      assert(file.nonEmpty)
      assert(file.get.name == "file1.txt")
      assert(new String(file.get.content, "UTF-8") == "Hello, World!")
    }
  }

  test("create a user and switch to the user") {
    val program = for {
      _ <- FileSystem.interpret(CreateUser("user1", "password1"))
      _ <- FileSystem.interpret(SwitchUser(User("user1", "password1")))
      currentUser <- FileSystem.interpret(WhoAmI)
    } yield currentUser

    runProgram(program).asserting { currentUser =>
      assert(currentUser.name == "user1")
      assert(currentUser.password == "password1")
    }
  }

  test("write and read a file with a new user") {
    val program = for {
      _    <- FileSystem.interpret(CreateUser("user2", "password2"))
      _    <- FileSystem.interpret(SwitchUser(User("user2", "password2")))
      _    <- FileSystem.interpret(CreateDirectory("/docs"))
      _    <- FileSystem.interpret(CreateFile("/docs", "file2.txt", "New content".getBytes("UTF-8"), ".txt", true))
      file <- FileSystem.interpret(ReadFile("/docs/file2.txt"))
    } yield file

    runProgram(program).asserting { file =>
      assert(file.nonEmpty)
      assert(file.get.name == "file2.txt")
      assert(new String(file.get.content, "UTF-8") == "New content")
    }
  }

  test("grant permissions and read a file with another user") {
    val program = for {
      _       <- FileSystem.interpret(CreateUser("user3", "password3"))
      _       <- FileSystem.interpret(CreateUser("user4", "password4"))
      _       <- FileSystem.interpret(SwitchUser(User("user3", "password3")))
      _       <- FileSystem.interpret(CreateDirectory("/docs"))
      _       <- FileSystem.interpret(CreateFile("/docs", "file3.txt", "Shared content".getBytes("UTF-8"), ".txt", true))
      _       <- FileSystem.interpret(GrantPermissions("/docs", "user4", Set(Read))) // Granting read permission to the directory
      _       <- FileSystem.interpret(GrantPermissions("/docs/file3.txt", "user4", Set(Read))) // Granting read permission to the file
      _       <- FileSystem.interpret(SwitchUser(User("user4", "password4")))
      fileOpt <- FileSystem.interpret(ReadFile("/docs/file3.txt"))
    } yield fileOpt

    runProgram(program).asserting { fileOpt =>
      assert(fileOpt.nonEmpty)
      assert(fileOpt.get.name == "file3.txt")
      assert(new String(fileOpt.get.content, "UTF-8") == "Shared content")
    }
  }

  test("delete a file and verify it is deleted") {
    val program = for {
      _       <- FileSystem.interpret(CreateDirectory("/docs"))
      _       <- FileSystem.interpret(CreateFile("/docs", "file4.txt", "To be deleted".getBytes("UTF-8"), ".txt", true))
      _       <- FileSystem.interpret(Delete("/docs/file4.txt"))
      fileOpt <- FileSystem.interpret(ReadFile("/docs/file4.txt"))
    } yield fileOpt

    runProgram(program).asserting { fileOpt =>
      assert(fileOpt.isEmpty)
    }
  }

  test("list users") {
    val program = for {
      _     <- FileSystem.interpret(CreateUser("user5", "password5"))
      _     <- FileSystem.interpret(CreateUser("user6", "password6"))
      users <- FileSystem.interpret(ListUsers)
    } yield users

    runProgram(program).asserting { users =>
      assert(users.exists(_.name == "user5"))
      assert(users.exists(_.name == "user6"))
    }
  }

  test("move a file") {
    val program = for {
      _       <- FileSystem.interpret(CreateDirectory("/docs"))
      _       <- FileSystem.interpret(CreateFile("/docs", "file5.txt", "Content to be moved".getBytes("UTF-8"), ".txt", true))
      _       <- FileSystem.interpret(CreateDirectory("/archive"))
      _       <- FileSystem.interpret(Move("/docs/file5.txt", "/archive/file5.txt", User("root", "rootpassword")))
      fileOpt <- FileSystem.interpret(ReadFile("/archive/file5.txt"))
    } yield fileOpt

    runProgram(program).asserting { fileOpt =>
      assert(fileOpt.nonEmpty)
      assert(fileOpt.get.name == "file5.txt")
      assert(new String(fileOpt.get.content, "UTF-8") == "Content to be moved")
    }
  }

  test("rename a file") {
    val program = for {
      _       <- FileSystem.interpret(CreateDirectory("/docs"))
      _       <- FileSystem.interpret(CreateFile("/docs", "file6.txt", "Content to be renamed".getBytes("UTF-8"), ".txt", true))
      _       <- FileSystem.interpret(Rename("/docs/file6.txt", "/docs/file6-renamed.txt", recursive = false))
      fileOpt <- FileSystem.interpret(ReadFile("/docs/file6-renamed.txt"))
    } yield fileOpt

    runProgram(program).asserting { fileOpt =>
      assert(fileOpt.nonEmpty)
      assert(fileOpt.get.name == "file6-renamed.txt")
      assert(new String(fileOpt.get.content, "UTF-8") == "Content to be renamed")
    }
  }

  test("change directory") {
    val program = for {
      _   <- FileSystem.interpret(CreateDirectory("/workspace"))
      _   <- FileSystem.interpret(Cd("/workspace"))
      pwd <- FileSystem.interpret(Pwd())
    } yield pwd

    runProgram(program).asserting { pwd =>
      assert(pwd == "/workspace")
    }
  }

  test("switch user and validate actions") {
    val program = for {
      _           <- FileSystem.interpret(CreateUser("admin", "adminpass"))
      _           <- FileSystem.interpret(SwitchUser(User("admin", "adminpass")))
      _           <- FileSystem.interpret(CreateDirectory("/adminDocs"))
      _           <- FileSystem.interpret(CreateFile("/adminDocs", "adminFile.txt", "Admin only".getBytes("UTF-8"), ".txt", true))
      currentUser <- FileSystem.interpret(WhoAmI)
    } yield currentUser

    runProgram(program).asserting { currentUser =>
      assert(currentUser.name == "admin")
      assert(currentUser.password == "adminpass")
    }
  }
}