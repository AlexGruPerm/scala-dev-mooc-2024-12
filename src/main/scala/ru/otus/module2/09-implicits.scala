package ru.otus.module2

object implicits {

  // implicit conversions

  object implicit_conversions {

    /** Расширить возможности типа String, методом trimToOption, который возвращает Option[String]
      * если строка пустая или null, то None
      * если нет, то Some от строки со всеми удаленными начальными и конечными пробелами
      */

     class StringOps(str: String){
      def trimToOption: Option[String] =
        Option(str).map(_.trim).filter(_.nonEmpty)
     }

     implicit def strToStringOps(s: String): StringOps = new StringOps(s)

      "foo".trimToOption

    // implicit conversions ОПАСНЫ


//    implicit def strToInt(s: String): Int = Integer.parseInt(s)
//
//    val r = "56" / 42






   implicit val seq = Seq("a", "b", "c")  // (a: Int): String



    def log(str: String) = println(str)

    log(1)


  }

  object implicit_scopes {

    trait Printable

    trait Printer[T] extends Printable {
      def print(v: T): Unit
    }

    object Printable {
       implicit val v: Printer[Bar] = new Printer[Bar] {
         override def print(v: Bar): Unit = println(s"Implicit from companion object Printable + $v")
       }
    }

    // companion object Printer
    object Printer {
       implicit val v: Printer[Bar] = new Printer[Bar] {
         override def print(v: Bar): Unit = println(s"Implicit from companion object Printer + $v")
       }
    }

    case class Bar()

    case class Foo()


    // companion object Bar
    object Bar {
//        implicit val v: Printer[Bar] = new Printer[Bar] {
//          override def print(v: Bar): Unit = println(s"Implicit from companion object Bar + $v")
//        }
    }

    // some arbitrary object
    object wildcardImplicits {
//      implicit val v: Printer[Bar] = new Printer[Bar] {
//        override def print(v: Bar): Unit = println(s"Implicit from wildcard import + $v")
//      }
    }

    import  wildcardImplicits._

    def print[T](b: T)(implicit m: Printer[T]) = m.print(b)

//     implicit val v1 = new Printer[Bar]{
//       def print(v: Bar): Unit = println(s"Implicit from local val + $v")
//     }


    val result = print(Bar())


  }


}
