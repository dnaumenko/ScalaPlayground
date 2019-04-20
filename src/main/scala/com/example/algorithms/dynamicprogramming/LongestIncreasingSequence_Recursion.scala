package com.example.algorithms.dynamicprogramming

import scala.annotation.tailrec

object LongestIncreasingSequence_Recursion extends App {
  def find(list: List[Int]): Int = {
    def doFind(list: List[Int], maxSoFar: Int, index: Int, solution: Int): Int = {
      if (index == list.length)
        solution
      else {
        val l1 = doFind(list, maxSoFar, index + 1, solution)
        val l2 = if (list(index) > maxSoFar) {
          doFind(list, list(index), index + 1, solution + 1)
        } else {
          0
        }

        l1.max(l2)
      }
    }

    doFind(list, 0, 0, 0)
  }

  println(find(List(2, 4, 3, 5, 1, 7, 6, 9, 8)))
}
