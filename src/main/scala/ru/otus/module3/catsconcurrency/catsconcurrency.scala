package ru.otus.module3.catsconcurrency
import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref, Resource}
//import ru.otus.module3.catsconcurrency.RefExample.program
import scala.concurrent.duration._
import cats.syntax.either._
import cats.implicits.catsSyntaxTuple2Parallel
import scala.util.Try

/*
object ResourceExample extends IOApp {

  val resource: Resource[IO, String] = Resource.make(
    IO(println("open resource")) *> IO("resource")
  )(res => IO(println(s"close resource: $res")))

  def run(args: List[String]): IO[ExitCode] =
    resource.use{ res =>
      for {
        _ <- IO(println("working with resource"))
        _ <- IO(println("doing something"))
      } yield ExitCode.Success

    }
}

object RefExample extends IOApp {
  val program: IO[Unit] = for {
    ref <- Ref[IO].of(0)
    _ <- ref.update(_+1)
    _ <- IO(println("update value in main thread"))
    fiber <- ref.update(_*2).start
    _ <- fiber.join
    value <- ref.get
    _ <- IO(println(s"value of : $value"))
  }  yield()


  def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}


object DeferredExample extends IOApp {
  val program: IO[Unit] = for {
    deffered <- Deferred[IO, Int]
    _ <- IO(println("vating for value"))
    fiber <- deffered.get.start
    _ <- IO.sleep(1.second)
    _ <- deffered.complete(42)
    _ <- fiber.join
    value <- deffered.get
    _ <- IO(println(s"got value: $value"))

  }  yield()

  def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}
*/

// main app with fibers
object  MainCatsConcurrency extends IOApp.Simple {
  //1
/*  def program: IO[Unit] = for {
    cmd <- IO.readLine
    _ <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(command)
    }
  } yield()

  def process(command: Command): IO[Unit] =
    command match {
      case Command.ReleaseTheDogs => ???
      case Command.LaunchDog(name) => ???
      case Command.ReadNumber => ???
      case Command.AddNumber(num) => ???
      case Command.Exit => { IO.println("Bye")}
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text))
      }
    }
*/
  //2 command
/*
  def program: IO[Unit] = for {
    cmd <- IO.readLine
    _ <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(command).flatMap{
        case true => program
        case false => IO.unit
      }
    }
  } yield()

  def process(command: Command): IO[Boolean] =
    command match {
      case Command.ReleaseTheDogs => ???
      case Command.LaunchDog(name) => ???
      case Command.ReadNumber => ???
      case Command.AddNumber(num) => ???
      case Command.Exit => { IO.println("Bye").as(false)}
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=> true)
      }
    }
*/

  //3 remove rec.
  /*
  def program(counter: Ref[IO, Int]): IO[Unit] = iteration(counter).iterateWhile(a=>a).void
  def iteration(counter: Ref[IO, Int]): IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProcced <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(counter)(command)
    }
  } yield shouldProcced


  def process(counter: Ref[IO, Int])(command: Command): IO[Boolean] =
    command match {
      case Command.ReleaseTheDogs => ???
      case Command.LaunchDog(name) => ???
      case Command.ReadNumber => ???
      case Command.AddNumber(num) => ???
      case Command.Exit => { IO.println("Bye").as(false)}
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=> true)
      }
    }


  def run: IO[Unit] = for {
    counter <- Ref.of[IO, Int](0)
    _ <- program(counter)
  } yield ()
*/

  //4 add pattern environment
/*  final case class Environment(counter: Ref[IO, Int])


  def program(env: Environment): IO[Unit] = iteration(env).iterateWhile(a=>a).void


  def iteration(env: Environment): IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProcced <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProcced


  def process(env: Environment)(command: Command): IO[Boolean] =
    command match {
      case Command.ReleaseTheDogs => ???
      case Command.LaunchDog(name) => ???
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.AddNumber(num) => env.counter.updateAndGet(_+num).flatMap(IO.println).as(true)
      case Command.Exit => { IO.println("Bye").as(false)}
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=> true)
      }
    }


  def run: IO[Unit] = for {
    counter <- Ref.of[IO, Int](0)
    _ <- program(Environment(counter))
  } yield ()
 */

  //5 from env to resource
/*
  final case class Environment(counter: Ref[IO, Int])


  def program(env: Environment): IO[Unit] = iteration(env).iterateWhile(a=>a).void


  def iteration(env: Environment): IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProcced <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProcced


  def process(env: Environment)(command: Command): IO[Boolean] =
    command match {
      case Command.ReleaseTheDogs => ???
      case Command.LaunchDog(name) => ???
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.AddNumber(num) => env.counter.updateAndGet(_+num).flatMap(IO.println).as(true)
      case Command.Exit => { IO.println("Bye").as(false)}
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=> true)
      }
    }

  def buildEnv: Resource[IO, Environment] = {
    val counter = Resource.make(IO.println("alloc. counter") *> Ref.of[IO, Int](0))(_ =>
    IO.println("dealloc. counter"))

    counter.map(Environment)
  }


  def run: IO[Unit] = buildEnv.use(env => program(env))
*/

  //6 final step

  final case class Environment(counter: Ref[IO, Int], startGun: Deferred[IO, Unit])


  def program(env: Environment): IO[Unit] = iteration(env).iterateWhile(a=>a).void


  def iteration(env: Environment): IO[Boolean] = for {
    cmd <- IO.print("> ") *> IO.readLine
    shouldProcced <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"Invalid command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProcced


  def process(env: Environment)(command: Command): IO[Boolean] =
    command match {
      case Command.ReleaseTheDogs => env.startGun.complete()
      case Command.LaunchDog(name) =>
        val fiberIO = (IO.println(s"Dog $name is ready")*> env.startGun.get *>
          IO.println(s"Dog $name is starting") *> env.counter.updateAndGet(_+1)
          .flatMap(value => IO.println(s"Dog $name observe value $value")))
        fiberIO.start.as(true)
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.AddNumber(num) => env.counter.updateAndGet(_+num).flatMap(IO.println).as(true)
      case Command.Exit => { IO.println("Bye").as(false)}
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=> true)
      }
    }

  def buildEnv: Resource[IO, Environment] = (
    Resource.make(IO.println("alloc. counter") *> Ref.of[IO, Int](0))(_ =>
      IO.println("dealloc. counter")
    ), Resource.make(IO.println("Alloc. gun") *> Deferred[IO, Unit])(
      _=> IO.println("Dealloc. gun")
    )).parMapN{case (counter, gun) => Environment(counter, gun)}


  def run: IO[Unit] = buildEnv.use(env => program(env))


}

sealed trait Command extends Product with Serializable


object Command {
  case object Echo extends Command
  case object Exit extends Command
  case class AddNumber(num: Int) extends  Command
  case object ReadNumber extends Command

  case class LaunchDog(name: String) extends Command
  case object ReleaseTheDogs extends Command

  def parse(s: String): Either[String, Command] =
    s.toLowerCase match {
      case "echo" => Echo.asRight
      case "exit" => Exit.asRight
      case "release-the-dogs" => ReleaseTheDogs.asRight
      case "read-number" => ReadNumber.asRight
      case cmd =>
        cmd.split(" ").toList match {
          case List("launch-dog", dogName) =>
            LaunchDog(dogName).asRight
          case List("add-number", IntString(num)) =>
            AddNumber(num).asRight
          case _ =>
            s"command $s could not be recognized".asLeft
        }
    }

  private object IntString {
    def unapply(s: String): Option[Int] =
      Try(s.toInt).toOption
  }
}

