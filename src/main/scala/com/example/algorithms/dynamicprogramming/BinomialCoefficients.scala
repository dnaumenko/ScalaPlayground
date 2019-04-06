package com.example.algorithms.dynamicprogramming

import com.example.algorithms.utils
import com.example.algorithms.utils.Timed

object BinomialCoefficients extends App {
  def binomCoeff(n: Int, k: Int): Int = { // how many ways to choose k things out of n possibilities
    val coeffs = Array.ofDim[Int](n + 1, n + 1)

    val points = for {
      i <- 0.to(n)
      j <- 0.to(n)
    } yield (i, j)

    points.foreach {
      case (i, 0) => coeffs(i)(0) = 1 // basic case, how many ways to choose 0 things out of i? Only one, it's empty set!
      case (i, j) if i == j => coeffs(i)(j) = 1 // basic case, how many ways to choose k things of of k? Only one! k itself!
      case (i, j) if i > 0 && j > 0 =>
        // recursion based on Pascal triangle, sum of two numbers directly above the given number
        coeffs(i)(j) = coeffs(i - 1)(j - 1) + coeffs(i -1)(j)
      case (i, j) => coeffs(i)(j) = 0
    }

    coeffs(n)(k)
  }

  println(binomCoeff(5, 4))
}

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