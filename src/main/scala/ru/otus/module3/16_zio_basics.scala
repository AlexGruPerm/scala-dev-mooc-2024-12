package ru.otus.module3

import zio._

import scala.concurrent.Future
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.Try



/** **
 * ZIO[-R, +E, +A] ----> R => Either[E, A]
 *
 */


object toyModel {


  /**
   * Используя executable encoding реализуем свой zio
   */

  case class ZIO[-R, +E, +A](run: R => Either[E, A]){

    def map[B](f: A => B): ZIO[R, E, B] =
      flatMap(a => ZIO(r => Right(f(a))))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO(r => this.run(r).fold(
        e => ZIO.fail(e).run(r),
        v => f(v).run(r)
      ))
  }


  /**
   * Реализуем конструкторы под названием effect и fail
   */

  object ZIO{
    type Task[T] = ZIO[Any, Throwable, T]

    def success[A](v: A): Task[A] = ZIO(_ => Right(v))

    def effect[A](a: => A): Task[A] = try{
      ZIO(_ => Right(a))
    } catch {
      case e: Throwable => fail(e)
    }

    def fail[E](e: E): ZIO[Any, E, Nothing] = ZIO( _ => Left(e))
  }

  val z1: ZIO[Any, Throwable, Unit] = ZIO.effect(println("Как тебя зовут?"))


  /** *
   * Напишите консольное echo приложение с помощью нашего игрушечного ZIO
   */
    val readFromConsole: ZIO[Any, Throwable, String] = ZIO.effect(StdIn.readLine())
    def writeToConsole(str: String): ZIO[Any, Throwable, Unit] =
      ZIO.effect(println(str))

    val echo: ZIO[Any, Throwable, Unit] =readFromConsole.flatMap(writeToConsole)

    val echo2: ZIO[Any, Throwable, Unit] = for{
      str <- readFromConsole
      _ <- writeToConsole(str)
    } yield ()



  type Error
  type Environment



  lazy val _: Task[Int] = ??? // ZIO[Any, Throwable,Int]
  lazy val _: IO[Error, Int] = ??? // ZIO[Any, Error, Int]
  lazy val _: RIO[Environment, Int] = ??? // ZIO[Environment, Throwable, Int]
  lazy val _: URIO[Environment, Int] = ??? // ZIO[Environment, Nothing, Int]
  lazy val _: UIO[Int] = ??? // ZIO[Any, Nothing, Int]
}

object zioConstructors {


  // не падающий эффект

  val z1: UIO[Int] = ZIO.succeed(7)

  // любой эффект

  val z2: Task[Unit] = ZIO.attempt(println("Hello"))


  val z3: IO[Throwable, Nothing] = ZIO.fail(new Throwable("Ooops"))

  def getUserNameByIdAsync(id: Int)(cb: Option[String] => Unit): Unit = {
    val name = if(id == 1) Some("Alex") else None
    cb(name)
  }

  val z4: ZIO[Any, Throwable, String] = ZIO.async{ callback =>
    getUserNameByIdAsync(1) {
      case Some(value) => callback(ZIO.succeed(value))
      case None => callback(ZIO.fail(new Throwable("User not found")))
    }
  }



  // Из Future
  lazy val f: Future[Int] = ???
  val z5: Task[Int] = ZIO.fromFuture(implicit ec => f.map(i => i + 1))


  // Из try
  lazy val t: Try[String] = ???
  val z6: Task[String] = ZIO.fromTry(t)



  // Из option
  lazy val opt : Option[Int] = ???
  val z7: IO[Option[Nothing], Int] = ZIO.fromOption(opt)
  val z8: UIO[Option[Int]] = z7.option
  val z9: IO[Option[Nothing], Int] = z8.some


  type User
  type Address


  def getUser(): Task[Option[User]] = ???
  def getAddress(u: User): Task[Option[Address]] = ???

  val r: ZIO[Any, Option[Throwable], Address] = for{
    user <- getUser().some
    address <- getAddress(user).some
  } yield address


  // Из either
  lazy val e: Either[String, Int] = ???
  val z10: IO[String, Int] = ZIO.fromEither(e)
  val z11: UIO[Either[String, Int]] = z10.either
  val z12 = z11.absolve


  // особые версии конструкторов

  ZIO.unit // ZIO[Unit]
  ZIO.none // ZIO[Option[Nothing]]
  ZIO.never // UIO[Nothing]
  ZIO.die(new Throwable("")) // UIO[Nothing]


}



object zioOperators {

  /** *
   *
   * 1. Создать ZIO эффект который будет читать строку из консоли
   */

  lazy val readLine = ???

  /** *
   *
   * 2. Создать ZIO эффект который будет писать строку в консоль
   */

  def writeLine(str: String) = ???

  /** *
   * 3. Создать ZIO эффект котрый будет трансформировать эффект содержащий строку в эффект содержащий Int
   */

  lazy val lineToInt = ???
  /** *
   * 3.Создать ZIO эффект, который будет работать как echo для консоли
   *
   */

  lazy val echo = ???

  /**
   * Создать ZIO эффект, который будет привествовать пользователя и говорить, что он работает как echo
   */

  lazy val greetAndEcho = ???



  // greet and echo улучшенный
  lazy val _: ZIO[Any, Throwable, Unit] = ???


  /**
   * Используя уже созданные эффекты, написать программу, которая будет считывать поочереди считывать две
   * строки из консоли, преобразовывать их в числа, а затем складывать их
   */

  lazy val r1 = ???

  /**
   * Второй вариант
   */

  lazy val r2: ZIO[Any, Throwable, Int] = ???

  /**
   * Доработать написанную программу, чтобы она еще печатала результат вычисления в консоль
   */

  lazy val r3 = ???


  lazy val a: Task[Int] = ???
  lazy val b: Task[String] = ???

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab1: ZIO[Any, Throwable, (Int, String)] = a zip b

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab2: ZIO[Any, Throwable, Int] = a zipLeft b

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab3: ZIO[Any, Throwable, String] = a zipRight b


  /**
   * Последовательная комбинация эффекта b и b, при этом результатом должна быть конкатенация
   * возвращаемых значений
   */
  lazy val ab4 = b.zipWith(b)(_ + _)




  /**
    * 
    * A as B
    */

  lazy val c = ???

}
