package ru.otus.module1

import ru.otus.module1.utils.NameableThreads

import java.io.File
import java.util.concurrent.{Executor, ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

object threads {


  // Thread

  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from: " +
        s"${Thread.currentThread().getName}")
    }
  }

  def printRunningTime(f: => Unit): Unit = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
  }

  def async(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def getRatesLocation1 = {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }
  def getRatesLocation2 = {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def getRatesLocation3 = async{
    Thread.sleep(1000)
    println("GetRatesLocation3")
  }
  def getRatesLocation4 = async{
    Thread.sleep(2000)
    println("GetRatesLocation4")
  }

  def async2[A](f: => A): A = {
    var v: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = v = f
    }
    t.start()
    t.join()
    v
  }

  def getRatesLocation5: Int = async2{
    Thread.sleep(1000)
    println("GetRatesLocation5")
    10
  }
  def getRatesLocation6: Int = async2{
    Thread.sleep(2000)
    println("GetRatesLocation6")
    20
  }

  class ToyFuture[T] private(v: () => T){

    private var r: Try[T] = null.asInstanceOf[Try[T]]
    private var isCompleted: Boolean = false
    private val q = mutable.Queue[Try[T] => _]()

    def onComplete[U](f: Try[T] => U): Unit = {
      if(isCompleted) f(r)
      else q.enqueue(f)
    }

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???
    def map[B](f: T => B): ToyFuture[B] = ???

    def start(executor: Executor) = {
      val t = new Runnable {
        override def run(): Unit = {
          val result = Try(v())
          r = result
          isCompleted = true
          while (q.nonEmpty){
            q.dequeue()(result)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](f: => T)(implicit executor: Executor): ToyFuture[T] = {
      val tf = new ToyFuture(() => f)
      tf.start(executor)
      tf
    }
  }

  implicit val ec = executor.pool1
  def getRatesLocation7: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    throw new Exception("oops")
    println("GetRatesLocation5")
    10
  }
  def getRatesLocation8: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation6")
    20
  }
}










object executor {
      val pool1: ExecutorService =
        Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService =
        Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService =
        Executors.newWorkStealingPool(4)
      val pool4: ExecutorService =
        Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}


object try_{

  def readFromFile(): List[String] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val result: List[String] = try{
      s.getLines().toList
    } catch {
      case e =>
        println(e.getMessage)
        Nil
    } finally {
      s.close()
    }
    result
  }

  def readFromFile2(): Try[List[String]] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val r = Try(s.getLines().toList)
    s.close()
    r
  }

  def readFromFile3(): Try[List[String]] = {
    val source: Try[BufferedSource] = Try(Source.fromFile(new File("ints.txt")))
    def lines(s: Source): Try[List[String]] = Try(s.getLines().toList)

//    val r: Try[List[String]] = for{
//      s <- source
//      l <- lines(s)
//    } yield l

    val r = source.flatMap(s => lines(s))
    source.foreach(_.close())
    r
  }

}






object future{
  // constructors

  implicit val ec = scala.concurrent.ExecutionContext.global
  val f1: Future[Int] = Future(1 + 1)(ec)
  val f2 = Future.successful(2 + 2)
  val f3 = Future.failed(new Throwable("oops"))

  val f4 = f1.map(_ + 2)(ec)
  val f5 = f1.flatMap(i => Future.successful(i + 2))(ec)

  f1.foreach(println)

  f1.onComplete {
    case Failure(exception) =>
      println(exception.getMessage)
    case Success(value) =>
      println(value)
  }



  // Execution context
  lazy val ec1 = ExecutionContext.fromExecutor(executor.pool1)
  lazy val ec2 = ExecutionContext.fromExecutor(executor.pool2)
  lazy val ec3 = ExecutionContext.fromExecutor(executor.pool3)
  lazy val ec4 = ExecutionContext.fromExecutor(executor.pool4)


  // combinators
  def longRunningComputation: Int = ???

  def getRatesLocation1 = Future{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(ec1)
  def getRatesLocation2 = Future{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(ec1)

//  def printRunningTime(f: => Unit): Unit = {
//    val start = System.currentTimeMillis()
//    f
//    val end = System.currentTimeMillis()
//    println(s"Running time: ${end - start}")
//  }


  def printRunningTime[T](f: => Future[T]): Future[T] = for{
    start <- Future.successful(System.currentTimeMillis())
    v <- f
    end <- Future.successful(System.currentTimeMillis())
    _ <- Future.successful(println(s"Running time: ${end - start}"))
  } yield v








  def action(v: Int): Int = {
    Thread.sleep(1000)
    println(s"Action $v in ${Thread.currentThread().getName}")
    v
  }


  // Execution contexts

  val f01 = Future(action(10))(ec1)
  val f02 = Future(action(20))(ec2)

  val f03 = f01.flatMap{v1 =>
    action(50)
    f02.map{v2 =>
      action(v1 + v2)
    }(ec4)
  }(ec3)






}

object promise {

  val p: Promise[Int] = Promise[Int]
  p.isCompleted // false
  val f1: Future[Int] = p.future
  f1.isCompleted // false
  f1.foreach(println(_))(scala.concurrent.ExecutionContext.global)

  p.failure(new Throwable("Ooops"))
  p.complete(Try(10))
  p.isCompleted // true
  f1.isCompleted // true


  def flatMap[T, B](future: Future[T])(f: T => Future[B])(implicit ec: ExecutionContext): Future[B] = {
    val p = Promise[B]
    future.onComplete {
      case Failure(exception) => p.failure(exception)
      case Success(value) =>
        f(value).onComplete {
          case Failure(exception) => p.failure(exception)
          case Success(value) => p.complete(Try(value))
        }
    }
    p.future
  }

}