package ru.otus.module2

import ru.otus.module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  // 1 Type constructor

  trait JsonWriter[T]{
    def write(v: T): JsValue
  }

  object JsonWriter{

    // summoner
    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

    def from[T](f: T => JsValue): JsonWriter[T] = v => f(v)

    implicit val strJsonWriter: JsonWriter[String] = from[String](JsString)

    implicit val intJsonWriter: JsonWriter[Int] = from[Int](JsNumber(_))

    implicit def optJsonWriter[A](implicit ev: JsonWriter[A]): JsonWriter[Option[A]] =
      from[Option[A]] {
        case Some(value) => ev.write(value)
        case None => JsNull
      }
  }

  object jsonSyntax{
    implicit class JsonWriterSyntax[T](v: T){
      def toJson(implicit ev: JsonWriter[T]) = ev.write(v)
    }
  }

  def toJson[T: JsonWriter](v: T): JsValue = {
    JsonWriter[T].write(v)
  }

  import jsonSyntax._


  "cdvfvfv".toJson
  10.toJson

  toJson("vfbfbg")
  toJson(10)
  toJson(Option(10))
  toJson(Option("cdvfvf"))









  // 1 Type constructor
  trait Ordering[T]{
    def less(a: T, b: T): Boolean
  }

  object Ordering{
    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      override def less(a: A, b: A): Boolean = f(a, b)
    }
    // 2 implicit values
    implicit val intOrdering = from[Int](_ < _) // (a, b) => a < b

    implicit val strOrdering = from[String](_ < _)
  }


  class User

  object User{
    implicit val user: Ordering[User] = ???
  }

  val u1 = new User
  val u2 = new User

  // 3 Implicit argument
  def greatest[A](a: A, b: A)(implicit ev: Ordering[A]): A =
    if(ev.less(a, b)) b else a


  greatest(10, 5)
  greatest("ab", "bcd")
  greatest(u1, u2)

  // 1 type constructor
  trait Eq[T]{
    def ===(a: T, b: T): Boolean
  }

  object Eq{

    def ===[T](a: T, b: T)(implicit ev: Eq[T]) = ev.===(a, b)

    implicit val strEq = new Eq[String]{
      override def ===(a: String, b: String): Boolean = a == b
    }

  }

  object eqSyntax{
    implicit class EqSyntax[T](a: T){
      def ===(b: T)(implicit ev: Eq[T]): Boolean = ev.===(a, b)
    }
  }

  import eqSyntax._



  val result = List("a", "b", "c").filter(str => str === 10)







}
