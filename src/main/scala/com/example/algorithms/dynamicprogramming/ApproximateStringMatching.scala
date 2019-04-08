package com.example.algorithms.dynamicprogramming

import com.example.algorithms.utils.Timed

import scala.annotation.tailrec

object ApproximateStringMatching extends App {
  def compareString(first: String, second: String, i: Int, j: Int): Int = {
    if (i == 0 && j == 0) return 1
    if (i < 0) return j
    if (j < 0) return i

    val matchCost = compareString(first, second, i - 1, j - 1) + matched(first.charAt(i), second.charAt(j))
    val insertCost = compareString(first, second, i, j - 1) + 1
    val deleteCost = compareString(first, second, i - 1, j) + 1


    Seq(matchCost, insertCost, deleteCost).min
  }

  private def matched(i: Char, j: Char): Int = if (i == j) 0 else 1

  val (first, second) = "mamamamamama" -> "papapapapapa"
  println(Timed(compareString(first, second, first.length - 1, second.length - 1)))
}

object ApproximateStringMatching_Cached extends App {
  case class Step(cost: Int, op: String)

  def compareString(first: String, second: String): (Int, String)  = {
    val maxLength = first.length.max(second.length)

    val pairsToCompare: Seq[(Int, Int)] = 1.until(first.length).flatMap {
      i => 1.until(second.length).map(j => (i, j))
    }

    val init = Array.ofDim[Step](maxLength, maxLength).map(_.map(_ => Step(0, "")))

    val result = pairsToCompare.foldLeft(init) {
      case (cache, (i, j)) =>
        val matchCost = (cache(i - 1)(j - 1).cost + matched(first.charAt(i), second.charAt(j))) -> "M"
        val insertCost = cache(i)(j - 1).cost + 1 -> "I"
        val deleteCost = cache(i - 1)(j).cost + 1 -> "D"

        val (minCost, op) = Seq(matchCost, insertCost, deleteCost).minBy(_._1)
        cache(i)(j) = Step(minCost, op)
        cache
    }

    result(first.length - 1)(second.length - 1).cost -> reconstructPath(first, second, result)
  }

  private def reconstructPath(first: String, second: String, cache: Array[Array[Step]]): String = {
    @tailrec
    def doReconstructPath(first: String, second: String, cache: Array[Array[Step]], i: Int, j: Int, path: String): String = {
      cache.lift(i).flatMap(_.lift(j)).map(_.op).getOrElse("NA") match {
        case "M" => doReconstructPath(first, second, cache, i - 1, j - 1, path + "M")
        case "I" => doReconstructPath(first, second, cache, i, j - 1, path + "I")
        case "D" => doReconstructPath(first, second, cache, i - 1, j, path + "D")
        case _ => path
      }
    }

    doReconstructPath(first, second, cache, first.length - 1, second.length - 1, "")
  }


  private def matched(i: Char, j: Char): Int = if (i == j) 0 else 1

  val (first, second) = " a" -> " aa"
  println(Timed(compareString(first, second)))
}