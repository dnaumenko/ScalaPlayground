package com.example.algorithms.combinatorial

import scala.io.Source

object TravelSalesmanProblem {
  case class Point(index: Int, x: Int, y: Int) // x,y coordinates, not latitude/longitude

  case class TspInstance(points: List[Point]) {
    def sq(i: Int): Int = i * i

    def calcDistance(prev: Point, next: Point): Double = {
      Math.sqrt(sq(prev.x - next.x) + sq(prev.y - prev.y))
    }

    def cost(): Double = {
      val (result, _) = points.tail.foldLeft((calcDistance(points.last, points.head), points.head)) {
        case ((cost, prevPoint), point) =>
          val distance = calcDistance(prevPoint, point)
          cost + distance -> point
      }
      result
    }

    override def toString: String = {
      s"Solution cost: ${cost()}\npoints (${points.size}): $points"
    }
  }

  def read(): TspInstance = {
    val map = Source.fromFile("./src/com/example/algorithms/combinatorial/tsp.txt").getLines().map(_.split(' ').toList).map {
      case index :: x :: y :: Nil => index.toInt -> Point(index.toInt, x.toInt, y.toInt)
    }.toMap

    val points = Source.fromFile("./src/com/example/algorithms/combinatorial/tsp.txt").getLines().map(_.split(' ').toList).map {
      case index :: x :: y :: Nil =>  Point(index.toInt, x.toInt, y.toInt)
    }.toList

    val best = Source.fromFile("./src/com/example/algorithms/combinatorial/tsp_solution.txt").getLines().map(_.toInt).map {
      index => map(index)
    }.toList

    println(s"Best: ${TspInstance(best)}")

    TspInstance(points)
  }
}
