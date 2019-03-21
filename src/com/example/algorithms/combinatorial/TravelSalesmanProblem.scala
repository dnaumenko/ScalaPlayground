package com.example.algorithms.combinatorial

import scala.io.Source

object TravelSalesmanProblem {
  case class Point(x: Int, y: Int) // x,y coordinates, not latitude/longitude

  case class TspInstance(points: Seq[Point]) {
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
      s"Solution cost: ${cost()}"
    }
  }

  def read(): TspInstance = {
    val points = Source.fromFile("./src/com/example/algorithms/combinatorial/tsp.txt").getLines().map(_.split(' ').toList).map {
      case _ :: x :: y :: Nil => Point(x.toInt, y.toInt)
    }

    TspInstance(points.toSeq)
  }
}
