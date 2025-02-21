package ru.otus.module2

import cats.{Id, Monad}
import cats.data.{Chain, Ior, Kleisli, NonEmptyChain, NonEmptyList, NonEmptyVector, OptionT, State, Validated, ValidatedNec, Writer, WriterT}
import cats.implicits._

import scala.concurrent.Future
import scala.util.Try



object functional {

  def sum(a: Int, b: Int): (String, Int) = {
    val result = a + b
    val log = s"Sum result: $result"
    (log, result)
  }

  def double(v: Int): (String, Int) = {
    val result = v * 2
    val log = s"Doubling result: $result"
    (log, result)
  }

  def sumAndDouble(a: Int, b: Int): (String, Int) = {
    val (l1, r1) = sum(a, b)
    val (l2, r2) = double(r1)
    (l1 ++ l2, r2)
  }

  println(sumAndDouble(2, 3))

  def sum2(a: Int, b: Int): Writer[Chain[String], Int] = {
    val result = a + b
      Writer.value[Chain[String], Int](result)
      .tell(Chain.one(s"Sum result: $result"))
  }

  def double2(v: Int): Writer[Chain[String], Int] = {
    val result = v * 2
    val log = Chain.one(s"Doubling result: $result")
    Writer(log, result)
  }

  def sumAndDouble2(a: Int, b: Int): Writer[Chain[String], Int] =
    sum2(a, b).flatMap(r1 => double2(r1))

  println(sumAndDouble2(2, 3).run)

  var counter = 0

  def step1() = {
    counter += 1
    s"Step 1 (${counter})"
  }

  def step2() = {
    counter += 1
    s"Step 2 (${counter})"
  }

  def step3() = {
    counter += 1
    s"Step 3 (${counter})"
  }

  def steps() = {
    step1() ++ step2() ++ step3()
  }

  println(steps())

  // State  S => (S1, A)

  def step1_2(): State[Int, String] = {
    State[Int, String](i => (i + 1, s"Step 1 (${i})"))
  }

  def step2_2() = {
    State[Int, String](i => (i + 1, s"Step 2 (${i})"))
  }

  def step3_2() = {
    State[Int, String](i => (i + 1, s"Step 3 (${i})"))
  }

  def steps2() = for{
    r1 <- step1_2()
    r2 <- step2_2()
    r3 <- step3_2()
  } yield r1 ++ r2 ++ r3

  println(steps2().run(1).map(_._2).value)

  // Kleisli
  // a => b b => c a => c

  val f1: Int => String = i => i.toString
  val f2: String => String = str => str + "_ooops"

  val f3: Int => String = f1 andThen f2

  val f4: String => Option[Int] = _.toIntOption
  val f5: Int => Option[Int] = i => Try(10 / i).toOption
  val f6: Kleisli[Option, String, Int] = Kleisli(f4) andThen Kleisli(f5)
  val _f6 = f6.run
  _f6("10") // Some(1)
  _f6("oops") // None


}



object dataStructures{

  // Chain

  val ch1 = Chain.one(1)
  val ch2 = Chain.empty[Int]
  val ch3 = Chain(2, 3)
  val ch4 = Chain.fromSeq(List(1, 2, 3))

  // операторы

  ch2 :+ 5
  5 +: ch2

  ch4.headOption

  // NonEmptyChain

  val nec: NonEmptyChain[Int] = NonEmptyChain.one(1)
  val ne2: NonEmptyChain[Int] = NonEmptyChain(1, 2)
  val nec3: Option[NonEmptyChain[Int]] =
    NonEmptyChain.fromSeq(List(1, 2))

  nec.head


}

object validation{

  type EmailValidationError = String
  type NameValidationError = String
  type AgeValidationError = String
  type Name = String
  type Email = String
  type Age = Int

  case class UserDTO(email: String, name: String, age: Int)
  case class User(email: String, name: String, age: Int)

  def emailValidatorE: Either[EmailValidationError, Email] = Left("Invalid email")
  def userNameValidatorE: Either[NameValidationError, Name] = Right("Bob")
  def ageValidatorE: Either[AgeValidationError, Age] = Left("Invalid age")

  def validatedUserData(userDTO: UserDTO): Either[String, User] = for{
    email <- emailValidatorE
    name <- userNameValidatorE
    age <- ageValidatorE
  } yield User(email, name, age)

  println(validatedUserData(UserDTO("scs", "cddv", 18)))

  // Validated

  val v1: Validated[String, Int] = Validated.valid[String, Int](10)
  val v2: Validated[String, Int] = Validated.invalid[String, Int]("Error")

  def emailValidatorV: Validated[EmailValidationError, Email] =
    Validated.invalid[String, String]("Invalid email")
  def userNameValidatorV: Validated[NameValidationError, Name] =
    Validated.valid[String, String]("Bob")
  def ageValidatorV: Validated[AgeValidationError, Age] =
    Validated.valid[String, Age](35)

//  def validatedUserDataV(userDTO: UserDTO): Validated[String, User] = for{
//    email <- emailValidatorV
//    name <- userNameValidatorV
//    age <- ageValidatorV
//  } yield User(email, name, age)

  def validatedUserDataV(userDTO: UserDTO): Validated[String, String] =
    emailValidatorV combine userNameValidatorV combine ageValidatorV.map(_.toString)

  def validatedUserDataV2(userDTO: UserDTO): Validated[String, User] =
    (emailValidatorV, userNameValidatorV, ageValidatorV).mapN{ (email, name, age) =>
      User(email, name, age)
    }

  def validatedUserDataV3(userDTO: UserDTO): ValidatedNec[String, User] =
    (emailValidatorV.toValidatedNec,
      userNameValidatorV.toValidatedNec,
      ageValidatorV.toValidatedNec).mapN{ (email, name, age) =>
      User(email, name, age)
    }

  println(validatedUserDataV3(UserDTO("scs", "cddv", 18)))

  // IoR

  val ior: Ior[String, User] = Ior.Left("Error")
  val ior2: Ior[String, User] = Ior.Right(User("", "", 35))
  val ior3: Ior[String, User] = Ior.Both("Warning", User("", "", 35))

  def emailValidatorIor: Ior[EmailValidationError, Email] = Ior.Left("Invalid email")
  def userNameValidatorIor: Ior[NameValidationError, Name] = Ior.Right("Bob")
  def ageValidatorIor: Ior[AgeValidationError, Age] = Ior.Both("Warning age", 18)

  def validatedUserDataIor(userDTO: UserDTO): Ior[String, User] = for{
    email <- emailValidatorIor
    name <- userNameValidatorIor
    age <- ageValidatorIor
  } yield User(email, name, age)
}



object transformers {

  val f1: Future[Int] = Future.successful(2)
  def f2(i: Int): Future[Option[Int]] = Future.successful(Try(10 / i).toOption)
  def f3(i: Int): Future[Option[Int]] = Future.successful(Try(10 / i).toOption)

  import scala.concurrent.ExecutionContext.Implicits.global

  val r: OptionT[Future, Int] = for{
    i1 <- OptionT.liftF(f1)
    i2 <- OptionT(f2(i1))
    i3 <- OptionT(f3(i2))
  } yield i2 + i3

  val _: Future[Option[Int]] = r.value


}