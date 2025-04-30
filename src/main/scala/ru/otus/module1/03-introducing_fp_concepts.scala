package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(x: Int, accum: Int): Int = {
      if( n <= 0) accum
      else loop(x - 1, x * accum)
    }
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции


  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  def sum(x: Int, y: Int): Int = x + y


  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 5



















}






/**
 *  Реализуем тип Option
 */



object opt {


  class Animal
  class Dog extends Animal
  class Cat extends Animal

  def treat(animal: Animal): Unit = ???
  def treat(animal: Option[Animal]): Unit = ???

  val d: Dog = ???
  val dOpt: Option[Dog] = ???
  treat(d)
  treat(dOpt)

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Variance
  // 1. Invariance
  // 2. Covariance
  // 3. Contrvariance

  trait Option[+T]{
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty option")
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = this match {
      case Some(v) => println(v)
      case None => ()
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](that: Option[B]): Option[(T,B)] = (this,that) match {
      case (Some(l),Some(r)) => Some((l,r))
      case _ => None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(f: T => Boolean) :Option[T] = this match {
      case Some(v) if f(v) => Some(v)
      case _ => this
    }


  }

  object Option{
    def apply[T](v: T): Option[T] = Some(v)
  }

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  val o0: Option[Int] = None
  val o1: Option[Int] = Option(1)
  val o2: Option[Int] = Option(2)
  val o3: Option[String] = Option("Str")
  o1.isEmpty // false

  o1.printIfAny()  //1

  println(o1.zip(o2)) //Some((1,2))
  println(o1.zip(o3)) //Some((1,Str))

  println(o1.filter(_ > 0)) //Some(1)
  println(o0.filter(_ > 0)) //None






  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

}

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */

  trait List[+T]{
    def ::[TT >: T](elem: TT): List[TT] = new ::(elem,this)

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[T] = {
      @tailrec
      def recurs(lst: List[T], accumulator: List[T] = List()): List[T] =
        lst match {
          case Nil => accumulator
          case ::(head,tail) => recurs(tail, new ::(head,accumulator))
        }
      recurs(this)
    }

    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: T => B): List[B] = {
      @tailrec
      def recurs(lst: List[T], accumulator: List[B] = List()): List[B] =
        lst match {
          case Nil => accumulator
          case ::(head,tail) => recurs(tail, new ::(f(head),accumulator) )
        }
      recurs(this).reverse
    }

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(f: T => Boolean): List[T] = {
      @tailrec
      def recurs(lst: List[T], accumulator: List[T] = List()): List[T] =
        lst match {
          case Nil => accumulator
          case ::(head,tail) if f(head) => recurs(tail, new ::(head,accumulator))
          case ::(_,tail) => recurs(tail, accumulator)
        }
      recurs(this).reverse
    }

    /**
     *
     * Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */
    def incList(lst: List[Int]): List[Int] = lst.map(_ + 1)

    /**
     *
     * Написать функцию shoutString котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */
    def shoutString(lst: List[String]): List[String] =
      lst.map("!" + _)

  }
  case class ::[T](head: T, tail: List[T]) extends List[T]
  case object Nil extends List[Nothing]

  object List{
    def apply[A](v: A*): List[A] = if(v.isEmpty) Nil
    else new ::(v.head, apply(v.tail:_*))
  }

  val l1: List[Nothing] = List()
  val l2 = List(1, 2, 3)

  val myList: List[Int] = List(1,2,3,4,5)
  val myListStr: List[String] = List("a","b","c")

  println(myList.map(_*2))                    // ::(2,::(4,::(6,::(8,::(10,Nil)))))
  println(myList.reverse)                     // ::(5,::(4,::(3,::(2,::(1,Nil)))))
  println(myList.filter(_ > 3))               // ::(4,::(5,Nil))
  println(myList.incList(myList))             // ::(2,::(3,::(4,::(5,::(6,Nil)))))
  println(myListStr.shoutString(myListStr))   // ::(!a,::(!b,::(!c,Nil)))



  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  /**
   *
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  /**
   *
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */


  /**
   *
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */


  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

}