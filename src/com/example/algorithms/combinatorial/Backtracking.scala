package com.example.algorithms.combinatorial

import com.example.algorithms.combinatorial.Backtracking._

object Backtracking  {
  /**
    * General form of backtracking algorithm
    * @tparam T - a type for an element in solution sequence.
    * @tparam Input - input type
    */
  trait BacktrackDFS[T, Input] {

    def isSolution(result: Seq[T], index: Int, input: Input): Boolean

    def computeCandidates(input: Input, result: Seq[T], index: Int): Seq[Seq[T]]

    def reportSolution(result: Seq[T]): Unit = println(result)

    def solve(input: Input, result: Seq[T], index: Int): Unit = {
      if (isSolution(result, index, input)) {
        reportSolution(result)
      } else {
        computeCandidates(input, result, index + 1).foreach { candidate =>
          // makeMove(result, index, input)
          solve(input, candidate, index + 1)
          // unmakeMove(result, index, input)
        }
      }
    }

    def generate(n: Input): Unit = {
      solve(input = n, result = Seq.empty, index = 0)
    }
  }

  // Type of solution element in sequence is Option[Int],
  // which means if Some(x) - x is in solution,
  // None - no element for this position.
  case class AllSubsetsProblem() extends BacktrackDFS[Option[Int], Int] {
    override def isSolution(result: Seq[Option[Int]], index: Int, input: Int): Boolean = {
      // input here is the input size, e.g. 3 means give me all subsets of size = 3
      // we stop when current index reached our problem size
      index == input
    }

    override def computeCandidates(input: Int, result: Seq[Option[Int]], index: Int): Seq[Seq[Option[Int]]] =
      Seq(
        result :+ Some(index),
        result :+ None
      )
  }


  case class AllPermutationsProblem() extends BacktrackDFS[Int, Int] {
    override def isSolution(result: Seq[Int], index: Int, input: Int): Boolean = {
      index == input
    }

    override def computeCandidates(input: Int, result: Seq[Int], index: Int): Seq[Seq[Int]] = {
      // will be faster if the result was some other data structure which has a better contains method performance
      1.to(input).filterNot(result.contains).map { i =>
        result :+ i
      }
    }
  }

  case class Node(value: String, var leafs: Seq[Node] = Seq.empty)

  case class Search(start: Node, end: Node)

  case class AllSimplePathsInGraph() extends BacktrackDFS[Node, Search] {
    override def isSolution(result: Seq[Node], index: Int, input: Search): Boolean = {
      result.lift(index - 1).exists(_.value == input.end.value)
    }

    override def reportSolution(result: Seq[Node]): Unit = {
      println(result.map(_.value))
    }

    override def computeCandidates(input: Search, result: Seq[Node], index: Int): Seq[Seq[Node]] = {
      if (index == 1) {
        Seq(result :+ input.start)
      } else {
        val visitedValues = result.map(_.value)
        result.last.leafs.filterNot(l => visitedValues.contains(l.value)).map { candidate =>
          result :+ candidate
        }
      }
    }
  }
}


object Main extends App {
  {
    AllPermutationsProblem().generate(3)
  }

  {
    val node1 = Node("1")
    val node2 = Node("2")
    val node3 = Node("3")
    val node4 = Node("4")
    val node5 = Node("5")
    val node6 = Node("6")

    node1.leafs = Seq(node2, node3, node4, node5)
    node2.leafs = Seq(node6)
    node3.leafs = Seq(node1, node4, node6)
    node4.leafs = Seq(node1, node3, node6)
    node5.leafs = Seq(node1, node6)
    node6.leafs = Seq(node2, node3, node4, node5)

    AllSimplePathsInGraph().generate(Search(start = node1, end = node3))
  }
}