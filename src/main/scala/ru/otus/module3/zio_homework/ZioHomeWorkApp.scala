package ru.otus.module3.zio_homework

import zio.{Random, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, durationInt}

import java.io.IOException

object ZioHomeWorkApp extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    //1.
     //zio.Runtime.default.run(guessProgram)
    /** Пример вывода:
     Введете целое значение от 1 до 3:
     10
     Вы ввели не корректное значение [10] должно быть (1,2,3)
     Введете целое значение от 1 до 3:
     abc
     Вы ввели не корректное значение [abc] должно быть (1,2,3)
     Введете целое значение от 1 до 3:
     1
     Вы не угадали случайное число. Число = 2 ваш ввод = 1
    */

    //2.
     //zio.Runtime.default.run(doWhile(testEffectForDoWhile(5)))
    /** Пример вывода:
     in=5 rv=3
     in=5 rv=3
     in=5 rv=8
     in=5 rv=5
     Выполняется эффект testEffectForDoWhile, пока его результат не будет true, число итераций м.б. разное
    */

    //3.
    //zio.Runtime.default.run(loadConfigOrDefault)
    /** Пример вывода:
     Ошибка загрузки конфига: (
     (
       (Missing data at host: Expected HOST to be set in the environment) or
       (Missing data at host: Expected host to be set in properties)
     ) and
     (
       (Missing data at port: Expected PORT to be set in the environment) or
       (Missing data at port: Expected port to be set in properties))
     )
     Используем дефолтный конфиг
     Успешно загружен конфиг: AppConfig(localhost,8080)
     */

    //4.1
     //zio.Runtime.default.run(eff)
    /** Пример вывода:
     Ожидание 1 секунды и вывод случайного числа от 1 до 10:
     random = 5
     */

    //4.2
     //zio.Runtime.default.run(ZIO.collectAll(effects))
    /** Пример вывода:
     random = 10
     random = 5
     random = 1
     random = 3
     random = 7
     random = 1
     random = 7
     random = 2
     random = 9
     random = 4
     Последовательные выполнение эффектов
     */

    //4.3
    //zio.Runtime.default.run(app)
    /** Пример вывода:
     random = 3
     random = 4
     random = 4
     random = 10
     random = 6
     random = 1
     random = 5
     random = 9
     random = 8
     random = 2
     sum = 52
     duration = 10173 ms. (т.к. эффекты запускались последовательно, то примерно 10*1 секунд.)
    */

    //4.4
    //zio.Runtime.default.run(printEffectRunningTime(appSpeedUp))
    /** Пример вывода:
     random = 2
     random = 6
     random = 6
     random = 2
     random = 1
     random = 7
     random = 4
     random = 7
     random = 10
     random = 9
     sum = 54
     duration = 1103 ms. (т.к. эффекты запускались параллельно, то примерно 1 секунда на все + немного накладного)
    */

    //5
     /*
    val prog: ZIO[Any,IOException,Unit] = zio.Console.printLine("Begin program") *>
      ZIO.sleep(2.second) *>
      zio.Console.printLine("End program")

      //Использование по аналогии с zio.Console.printLine
    val progWithTiming = for {
      _ <- PrintEffectRunningTime.runningTime(prog)
    } yield ()

    zio.Runtime.default.run(progWithTiming.provideLayer(PrintEffectRunningTime.live))
     */
    /** Пример вывода:
     Begin program
     End program
     duration = 2037 ms.

     После вывода "Begin program" ожидание 2 секунды и вывод "End program"
     */

    //6
    /*
    zio.Runtime.default.run(runApp.provideLayer(PrintEffectRunningTime.live).map(sum =>
      Console.println(s"Sum = $sum")))
    */
    /** Пример вывода:
     random = 3
     random = 8
     random = 10
     random = 4
     random = 5
     random = 1
     random = 1
     random = 4
     random = 8
     random = 1
     sum = 45
     duration = 10184 ms.
     Sum = 45
     */

  }

}
