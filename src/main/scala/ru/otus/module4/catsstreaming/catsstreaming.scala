package catsstreaming

import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource}
import fs2.{Chunk, Pure, Stream}

import java.time.Instant
import scala.concurrent.duration._
import cats.effect.unsafe.implicits.global
import cats.effect.kernel.Async


object Streams extends IOApp.Simple {

  //1
  val pureApply: Stream[Pure, Int] = Stream.apply(1,2,3)
  //2
  val ioApply: Stream[IO, Int] = pureApply.covary[IO]
  //3
  val list = List(1,2,3)
  val listEmits: Stream[Pure, Int] = Stream.emits(list)
  //4
  val a: Seq[Int] = pureApply.toList
  //5
  val aa: IO[Unit] = ioApply.compile.drain
  //6
  val unfolded = Stream.unfoldEval(0){s=>
    val next = s+10
    if (s>=50)
      IO.none
    else
      IO.println(next.toString).as(Some(next.toString, next))
  }
  //7
  val s = Stream.eval(IO.readLine).evalMap(s => IO.println(s">> $s")).repeatN(3)

  //8-9
  type Desccriptor = String
  def openFile: IO[Desccriptor] = IO.println("open file").as("file descriptor")
  def closeFile(descriptor: Desccriptor): IO[Unit] = IO.println("closing file")
  def redaFile(descriptor: Desccriptor): Stream[IO, Byte] = Stream.emits(s"file content".map(_.toByte).toArray)

  val fileResource: Resource[IO, Desccriptor] = Resource.make(openFile)(closeFile)

  val resourceStream: Stream[IO, Int] = Stream.resource(fileResource).flatMap(redaFile).map(b=>b.toInt + 100)
  val resourceStreamChunks: Stream[IO, Int] =
    Stream.resource(fileResource).flatMap(redaFile).map(b=>b.toInt + 100).rechunkRandomly()

  //10

  val fixedDelayStream = Stream.fixedDelay[IO](1.second).evalMap(_=> IO.sleep(2.seconds) *> IO.println(Instant.now()))
  //2025-04-09T17:48:24.663684200Z
  //2025-04-09T17:48:27.682806500Z
  //2025-04-09T17:48:30.685933800Z
  //2025-04-09T17:48:33.689090400Z
  val fixedRateStream = Stream.fixedRate[IO](1.second).evalMap(_ => IO.sleep(2.seconds) *>  IO.println(Instant.now()))
  //2025-04-09T17:49:33.371439500Z
  //2025-04-09T17:49:35.402137800Z
  //2025-04-09T17:49:37.404090100Z
  //2025-04-09T17:49:39.406980200Z
  //2025-04-09T17:49:41.409151Z
  //2025-04-09T17:49:43.413681Z

  //11
  val queueIO = cats.effect.std.Queue.bounded[IO, Int](100)
  def putInQueue(queue: Queue[IO, Int], value: Int) =
    queue.offer(value)

  val queueStreamIO: IO[Stream[IO, Int]] = for {
    q <- queueIO
    _ <- (IO.sleep(500.millis) *> putInQueue(q, 5).replicateA(10).start)
  } yield  Stream.fromQueueUnterminated(q)

  val queueStream = Stream.force(queueStreamIO)

  def increment(s: Stream[IO, Int]): Stream[IO, Int] = s.map(_+1)


  def run: IO[Unit] = {
    queueStream.through(increment)
      .evalTap(n=> IO.println(s"element: $n"))
      .compile.drain

  }

}
