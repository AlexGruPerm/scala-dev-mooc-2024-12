package ru.otus.module1.DataCollection1

import scala.util.Random

/**
 * Нужно проверить формулу условной вероятности опытным путем.
 * Изестно, что достаем 2 шарика без возвращения
 *
 * P(черный первый) = 3/6 = 1/2 = 0.5
 * P(белый второй | черный первый) = 3/5 = 0.6
 *
 * общая вероятность того, что первый шар черный, а второй белый:
 * P(черный первый и белый второй) = P(черный первый) × P(белый второй | черный первый) = 1/2 * 3/5 = 3/10 = 0.3
 *
 * Проводим эксперимент(многократно).
 * Достаем из набора 6 шаров по 2 шара случайно,
 * если это были: первый черный(0), второй белый(1), то считаем эксперимент успешным => true.
 * Считаем количество успешных экспериментов и долю успешных среди всех.
 *
 * Значения аналитической вероятности и опытной должны быть близки.
 *
 */
class BallsExperiment {
  /**
   * Генератор случайных величин
   */
  private val random = new Random()

  /**
   * Урна с шарами
   */
  private val BucketWithBalls: List[Int] = List(0,1,0,1,0,1)

  /**
   * Функция получает на вход урну с шарами,
   * достет случайно один шар.
   * Возвращает (цвет шара, корзину с шарами без изъятого шара)
   */
  private def getBallAndBucketRest(l: List[Int]): (Int,List[Int]) = {
    val index = random.nextInt(l.size)
    val color: Int = l(index)
    val rest: List[Int] = l.patch(index,Nil,1)
    (color,rest)
  }

  /**
   * Функция извлекает последовательно 2 шара из корзины.
   * Если это были шары: 1-й черный, 2-й белый, то вернет true, иначе false.
   * Используем lazy для secondColor, т.к. в if используется
   * короткое замыкание (не оценивает второй операнд, если первый ложен).
   */
  def isFirstBlackSecondWhite: Boolean = {
    val (firstColor,firstRest): (Int,List[Int]) = getBallAndBucketRest(BucketWithBalls)
    lazy val (secondColor,_): (Int,List[Int]) = getBallAndBucketRest(firstRest)
    if (firstColor == 0 && secondColor == 1)
      true
    else
      false
  }

}

/**
 * Аналитическая вероятность того, что первый шар будет черным, а второй — белым,
 * составляет 0.3
 */
object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 1000000
    val listOfExperiments: List[BallsExperiment] = (1 to count).map(_ => new BallsExperiment).toList
    val countOfExperiments: List[Boolean] = listOfExperiments.map(_.isFirstBlackSecondWhite)
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count) // 0.300429
  }
}