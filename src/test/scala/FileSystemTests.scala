import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import org.cameron.cs.FileSystem
import org.cameron.cs.FileSystem.{FileSystemOpVisitor, FileSystemOpVisitorFileSystemStateT, FileSystemState, FileSystemStateT, findEntity}
import org.cameron.cs.file.{Directory, File, FileMetadata}
import org.cameron.cs.ops.{Cd, CloseFile, CreateDirectory, CreateFile, CreateUser, Exit, GetJournal, GetSessions, GrantPermissions, ListUsers, Move, OpenFile, Pwd, ReadFile, ReadFileByFd, Remove, Rename, SwitchUser, Tree, WhoAmI, WriteFileByFd, WriteFileContent}
import org.cameron.cs.security.{Execute, Read, Write}
import org.cameron.cs.user.User
import org.scalatest.funsuite.AsyncFunSuite

import java.time.Instant

class FileSystemTests extends AsyncFunSuite with AsyncIOSpec {

  val initialRootUser = User("root", "rootpassword")
  val initialRoot = Directory("/", Map.empty, FileMetadata(initialRootUser, Map(initialRootUser.name -> Set(Read, Write, Execute)), Instant.now, Instant.now, initialRootUser.name, 0))
  val initialState = FileSystemState(initialRoot, initialRootUser, initialRoot, Map("root" -> initialRootUser))

  implicit val fileSystemOpVisitor: FileSystemOpVisitor[FileSystemStateT] = FileSystemOpVisitorFileSystemStateT

  def runProgram[A](program: FileSystemStateT[A]): IO[A] = {
    program.runA(initialState)
  }

  test("user session management") {
    val program = for {
      _ <- FileSystem.interpret(CreateUser("userTest1", "password1"))
      _ <- FileSystem.interpret(CreateUser("userTest2", "password2"))
      _ <- FileSystem.interpret(SwitchUser(User("userTest1", "password1")))
      _ <- FileSystem.interpret(CreateDirectory("/docs"))
      _ <- FileSystem.interpret(Cd("/docs"))
      _ <- FileSystem.interpret(Pwd)
      _ <- FileSystem.interpret(Exit)
      _ <- FileSystem.interpret(SwitchUser(User("userTest2", "password2")))
      _ <- FileSystem.interpret(CreateFile("/docs", "file1.txt", "txt", true))
      _ <- FileSystem.interpret(WriteFileContent("/docs", "file1.txt", "Hello, world!".getBytes))
      _ <- FileSystem.interpret(Exit)
      _ <- FileSystem.interpret(SwitchUser(User("root", "rootpassword")))
      sessions <- FileSystem.interpret(GetSessions)
    } yield sessions

    runProgram(program).asserting { sessions =>
      assert(sessions.size == 3)

      val user1SessionOpt = sessions.find(_.user.name == "userTest1")
      assert(user1SessionOpt.nonEmpty)

      val user1Session = user1SessionOpt.get
      assert(user1Session.commands.nonEmpty)
      assert(user1Session.commands == List("CreateDirectory(/docs)", "Cd(/docs)", "Pwd"))
      assert(user1Session.disconnectedAt.nonEmpty)

      val user2SessionOpt = sessions.find(_.user.name == "userTest2")
      assert(user2SessionOpt.nonEmpty)
      val user2Session = user2SessionOpt.get
      assert(user2Session.commands == List("CreateFile(/docs,file1.txt,txt,true)", "WriteFileContent(/docs,file1.txt)"))
      assert(user2Session.disconnectedAt.nonEmpty)

      val rootSessionOpt = sessions.find(_.user.name == "root")
      assert(rootSessionOpt.nonEmpty)
      val rootSession = rootSessionOpt.get
      assert(rootSession.commands == List("GetSessions"))
      assert(rootSession.disconnectedAt.isEmpty)
    }
  }

  test("command journaling for different users") {
    val program = for {
      _ <- FileSystem.interpret(CreateUser("user1", "password1"))
      _ <- FileSystem.interpret(CreateUser("user2", "password2"))
      _ <- FileSystem.interpret(SwitchUser(User("user1", "password1")))
      _ <- FileSystem.interpret(CreateDirectory("/docs"))
      _ <- FileSystem.interpret(Exit)
      _ <- FileSystem.interpret(SwitchUser(User("user2", "password2")))
      _ <- FileSystem.interpret(CreateFile("/docs", "file1.txt", "txt", true))
      _ <- FileSystem.interpret(WriteFileContent("/docs", "file1.txt", "Hello, world!".getBytes))
      _ <- FileSystem.interpret(Exit)
      _ <- FileSystem.interpret(SwitchUser(User("user1", "password1")))
      user1Journal <- FileSystem.interpret(GetJournal)
      _ <- FileSystem.interpret(SwitchUser(User("user2", "password2")))
      user2Journal <- FileSystem.interpret(GetJournal)
      _ <- FileSystem.interpret(SwitchUser(User("root", "rootpassword")))
      rootJournal <- FileSystem.interpret(GetJournal)
    } yield (user1Journal, user2Journal, rootJournal)

    runProgram(program).asserting { case (user1Journal, user2Journal, rootJournal) =>
      assert(user1Journal == List("CreateDirectory(/docs)", "GetJournal"))

      assert(user2Journal == List("CreateFile(/docs,file1.txt,txt,true)", "WriteFileContent(/docs,file1.txt)", "GetJournal"))

      assert(rootJournal == List(
        "CreateUser(user1,password1)",
        "CreateUser(user2,password2)",
        "SwitchUser(User(user1,password1))",
        "GetJournal"
      ))
    }
  }

  test("create and read a file") {
    val content = "Hello, World!".getBytes("UTF-8")
    val program = for {
      _    <- FileSystem.interpret(CreateDirectory("/docs"))
      _    <- FileSystem.interpret(CreateFile("/docs", "file1.txt", ".txt", true))
      _    <- FileSystem.interpret(WriteFileContent("/docs", "file1.txt", content))
      file <- FileSystem.interpret(ReadFile("/docs/file1.txt"))
    } yield file

    runProgram(program).asserting { file =>
      assert(file.nonEmpty)
      assert(file.get.name == "file1.txt")
      assert(new String(file.get.content, "UTF-8") == "Hello, World!")
      assert(file.get.metadata.size == content.length)
    }
  }

  test("create a user and switch to the user") {
    val program = for {
      _           <- FileSystem.interpret(CreateUser("user1", "password1"))
      _           <- FileSystem.interpret(SwitchUser(User("user1", "password1")))
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
      _    <- FileSystem.interpret(CreateFile("/docs", "file2.txt", ".txt", true))
      _    <- FileSystem.interpret(WriteFileContent("/docs", "file2.txt",  "New content".getBytes("UTF-8")))
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
      _       <- FileSystem.interpret(CreateFile("/docs", "file3.txt", ".txt", true))
      _       <- FileSystem.interpret(WriteFileContent("/docs", "file3.txt", "Shared content".getBytes("UTF-8")))
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
      _       <- FileSystem.interpret(CreateFile("/docs", "file4.txt", ".txt", true))
      _       <- FileSystem.interpret(WriteFileContent("/docs", "file4.txt", "To be deleted".getBytes("UTF-8")))
      _       <- FileSystem.interpret(Remove("/docs/file4.txt"))
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
      _       <- FileSystem.interpret(CreateFile("/docs", "file5.txt", ".txt", true))
      _       <- FileSystem.interpret(WriteFileContent("/docs", "file5.txt", "Content to be moved".getBytes("UTF-8")))
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
    val content = "Content to be renamed".getBytes("UTF-8")
    val program = for {
      _       <- FileSystem.interpret(CreateDirectory("/docs"))
      _       <- FileSystem.interpret(CreateFile("/docs", "file6.txt", ".txt", true))
      _       <- FileSystem.interpret(WriteFileContent("/docs", "file6.txt", content))
      _       <- FileSystem.interpret(Rename("/docs/file6.txt", "/docs/file6-renamed.txt", recursive = false))
      fileOpt <- FileSystem.interpret(ReadFile("/docs/file6-renamed.txt"))
    } yield fileOpt

    runProgram(program).asserting { fileOpt =>
      assert(fileOpt.nonEmpty)
      assert(fileOpt.get.name == "file6-renamed.txt")
      assert(new String(fileOpt.get.content, "UTF-8") == "Content to be renamed")
      assert(fileOpt.get.metadata.size == content.length)
    }
  }

  test("change directory") {
    val program = for {
      _   <- FileSystem.interpret(CreateDirectory("/workspace"))
      _   <- FileSystem.interpret(Cd("/workspace"))
      pwd <- FileSystem.interpret(Pwd)
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
      _           <- FileSystem.interpret(CreateFile("/adminDocs", "adminFile.txt", ".txt", true))
      _           <- FileSystem.interpret(WriteFileContent("/adminDocs", "adminFile.txt", "Admin only".getBytes("UTF-8")))
      currentUser <- FileSystem.interpret(WhoAmI)
    } yield currentUser

    runProgram(program).asserting { currentUser =>
      assert(currentUser.name == "admin")
      assert(currentUser.password == "adminpass")
    }
  }

  test("print tree structure") {
    val program = for {
      _    <- FileSystem.interpret(SwitchUser(initialRootUser))
      _    <- FileSystem.interpret(CreateDirectory("/docs"))
      _    <- FileSystem.interpret(CreateFile("/docs", "file1.txt", ".txt", true))
      _    <- FileSystem.interpret(WriteFileContent("/docs", "file1.txt", "Content of file1".getBytes("UTF-8")))
      _    <- FileSystem.interpret(CreateDirectory("/docs/subdocs"))
      _    <- FileSystem.interpret(CreateFile("/docs/subdocs", "file2.txt", ".txt", true))
      _    <- FileSystem.interpret(WriteFileContent("/docs/subdocs", "file2.txt", "Content of file2".getBytes("UTF-8")))

      tree <- FileSystem.interpret(Tree("/"))
      _    =  println(tree)
    } yield tree

    runProgram(program).asserting { treeStr => assert(treeStr.nonEmpty)}
  }

  test("open, write and read file by file descriptor") {
    val program = for {
      _       <- FileSystem.interpret(CreateFile("/docs", "file1.txt", "txt", true))
      fd      <- FileSystem.interpret(OpenFile("/docs/file1.txt"))
      _       <- FileSystem.interpret(WriteFileByFd(fd, "Hello, world!".getBytes("UTF-8")))
      content <- FileSystem.interpret(ReadFileByFd(fd))
      _       <- FileSystem.interpret(CloseFile(fd))
    } yield content

    runProgram(program).asserting {
      case Some(data) => assert(data.sameElements("Hello, world!".getBytes))
      case None       => fail("File content should not be None")
    }
  }
}