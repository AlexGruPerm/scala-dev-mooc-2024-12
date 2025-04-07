package ru.otus.module3

import ru.otus.module3.tryFinally.{future, traditional, zioResource}
import zio.ZIO.ifZIO
import zio._


object ZIOMain {

  def main(args: Array[String]): Unit = {
    val z1 = ZIO.attempt(println("Hello"))
    val environment: ZEnvironment[Console] =
      ZEnvironment[Console](Console.ConsoleLive)



    Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(
        ???
      )
    }

  }

}

object ZIOMain2 extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???

}


