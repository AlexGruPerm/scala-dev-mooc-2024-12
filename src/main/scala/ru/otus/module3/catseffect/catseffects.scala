package ru.otus.module3.catseffect

import cats.effect.unsafe.implicits.global
import cats.effect._
import cats.implicits._

import scala.concurrent.Future
import cats.{Functor, Group, Monad, Monoid, Semigroup}

import java.lang

object catseffects {
  def main(args: Array[String]): Unit = {

    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    /*
    val hello: IO[Unit] = IO(println("sdf"))
    hello.unsafeRunSync()


    val delayed: IO[Int] = IO.delay{
      42
    }
    val pureValue = IO.pure(42)


    val optionFunctor = Functor[Option]
    val res = optionFunctor.map(Some(2))(_*3)
    // fa.map(identity) == fa
    //fa.map(f).map(g) == fa.map(f.andThen(g))


//    trait Semigroup[A] {
//      def combine(x: A, y:A): A
//    }

    val sum = Semigroup[Int].combine(2,3)

//    trait Monoid[A] extends Semigroup[A]{
//      def empty: A
//    }

    val sum1 = Monoid[Int].combine(2,3)
    val sum1_empty = Monoid[Int].empty

//    combine(x, empty) == x
//    combine(empty,x) == x

//    trait Group[A] extends Monoid[A] {
//      def inverse(a: A): A
//    }


    val balance = 100
    val transaction = 20

    val newBalance = Group[Int].combine(balance, -transaction)

    val cancelBalance = Group[Int].combine(100, Group[Int].inverse(50))*/

    val pure = IO.pure("pure value")

    val sideeffect = IO.delay(println("111"))
    val mistake: IO[Unit] = IO.pure(println("error"))

    sideeffect.unsafeRunSync()
    sideeffect.unsafeRunSync()
    mistake.unsafeRunSync()
    mistake.unsafeRunSync()

    val fromEither = IO.fromEither(Left(new Exception("fail")))
    val fromFuture = IO.fromFuture(IO.delay(Future.successful(1)))

    val failing = IO.raiseError(new Exception("error"))
    val never = IO.never

    val future = Future(Thread.sleep(1000)).map(_=>100)
    val async = IO.async_(
      (cb: Either[Throwable, Int] => Unit) =>
        future.onComplete(a=>cb(a.toEither))
    )

    async.unsafeRunSync()
    async.flatMap(i=>IO.println(i)).unsafeRunSync()
  }
}


// example NO TaglessFinal
object FilesAndHttpIO extends IOApp.Simple {
  def readFile(file: String): IO[String] =
    IO.pure("content of some file")

  def httpPost(url: String, body: String): IO[Unit] =
    IO.delay(println(s"Post $url : $body"))

  def run: IO[Unit] = for {
    _ <- IO.delay(println("enter file path"))
    path <- IO.readLine
    data <- readFile(path)
    _ <- httpPost("sdfsdf.de", data)
  } yield ()
}

// example WITH TaglessFinal

//DSL
trait FileSystem[F[_]]{
  def readFile(path: String) : F[String]
}

object FileSystem {
  def apply[F[_]: FileSystem]: FileSystem[F] = implicitly
}

trait HttpClient[F[_]] {
  def postData(url: String, body: String): F[Unit]
}

object HttpClient{
  def apply[F[_]: HttpClient]: HttpClient[F] = implicitly
}

trait Console[F[_]] {
  def readLine: F[String]
  def printLine(s: String): F[Unit]
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly
}

//interpreter
object Interpreter {
  implicit val consoleIO: Console[IO] = new Console[IO] {
    override def readLine: IO[String] = IO.readLine

    override def printLine(s: String): IO[Unit] = IO.println(s)
  }

  implicit  val fileSystemIO: FileSystem[IO] = new FileSystem[IO] {
    override def readFile(path: String): IO[String] = IO.pure(s"some file with content $path")
  }

  implicit  val httpClientIO: HttpClient[IO] = new HttpClient[IO] {
    override def postData(url: String, body: String): IO[Unit] = IO.delay(println(s"Post $url: $body"))
  }
}

// bring all together
object FilesAndHttpTF extends IOApp.Simple {
  def program[F[_]: Console: Monad : FileSystem: HttpClient]: F[Unit] =
    for {
      _ <- Console[F].printLine("Enter file path: ")
      path <- Console[F].readLine
      data <- FileSystem[F].readFile(path)
      _ <- HttpClient[F].postData("xsdfsdf.de", data)
    }  yield ()

  import Interpreter.httpClientIO
  import Interpreter.fileSystemIO
  import Interpreter.consoleIO

  def run: IO[Unit] = program[IO]
}

