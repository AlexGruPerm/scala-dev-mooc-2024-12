package ru.otus.module3

import zio.{Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault}


object ZIOMain {

  def main(args: Array[String]): Unit = {
    val z1: Task[Unit] = ZIO.attempt(println("Hello"))
    zio.Runtime.default.run(z1)
  }

}

object ZIOMain2 extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.attempt(println("Hello"))
}


