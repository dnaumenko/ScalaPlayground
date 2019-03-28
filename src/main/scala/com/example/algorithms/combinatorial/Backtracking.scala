package com.example.algorithms.combinatorial

import com.example.algorithms.combinatorial.Backtracking._

object Backtracking  {
  /**
    * General form of backtracking algorithm
    * @tparam T - a type for an element in solution sequence.
    * @tparam Input - input type
    */
  trait BacktrackDFS[T, Input] {

    def isSolution(result: T, index: Int, input: Input): Boolean

    def computeCandidates(input: Input, result: T, index: Int): Seq[T]

    def reportSolution(result: T): Unit = println(result)

    def shouldContinue(): Boolean = true

    def solve(input: Input, result: T, index: Int): Unit = {
      if (isSolution(result, index, input)) {
        reportSolution(result)
      } else if (shouldContinue()) {
        computeCandidates(input, result, index + 1).foreach { candidate =>
          solve(input, candidate, index + 1)
        }
      }
    }
  }

  // Type of solution element in sequence is Option[Int],
  // which means if Some(x) - x is in solution,
  // None - no element for this position.
  case class AllSubsetsProblem() extends BacktrackDFS[Seq[Option[Int]], Int] {
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


  case class AllPermutationsProblem() extends BacktrackDFS[Seq[Int], Int] {
    override def isSolution(result: Seq[Int], index: Int, input: Int): Boolean = {
      index == input
    }

    override def computeCandidates(input: Int, result: Seq[Int], index: Int): Seq[Seq[Int]] = {
      // will be faster if the result was some other data structure which has a better contains method performance
      1.to(input).filterNot(result.contains).map { i =>
        result :+ i
      }
    }

    def generate(n: Int): Unit = {
      solve(input = n, result = Seq.empty, index = 0)
    }
  }

  case class Node(value: String, var leafs: Seq[Node] = Seq.empty)

  case class Search(start: Node, end: Node)

  case class AllSimplePathsInGraph() extends BacktrackDFS[Seq[Node], Search] {

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

    def generate(n: Search): Unit = {
      solve(input = n, result = Seq.empty, index = 0)
    }
  }

  case class Square(x: Int, y: Int, value: Int)
  case class Board(moves: Seq[Square]) {
    def size(): Int = 9 * 9

    def cells(): Array[Array[Int]] = {
      moves.foldLeft(Array.ofDim[Int](9, 9)) {
        case (cells, move) =>
          cells(move.x)(move.y) = move.value
          cells
      }
    }

    override def toString: String = {
      cells().map(_.mkString("|")).mkString("\n")
    }
  }
  object Board {
    def freeValues(x: Int, y: Int, cells: Array[Array[Int]]): Seq[Int] = {
      freeRowValues(x, cells).intersect(freeColValues(y, cells)).intersect(freeSegValues(x, y, cells))
    }

    def freeRowValues(x: Int, cells: Array[Array[Int]]): Seq[Int] = {
      (1 to 9).diff(cells(x).filter(_ != 0))
    }

    def freeColValues(y: Int, cells: Array[Array[Int]]): Seq[Int] = {
      (1 to 9).diff(cells.map(row => row(y)).filter(_ != 0))
    }

    def freeSegValues(x: Int, y: Int, cells: Array[Array[Int]]): Seq[Int] = {
      val segValues = {
        val (segX, segY) = (x / 3, y / 3)
        cells.zipWithIndex.filter {
          case (_, index) => index / 3 == segX
        }.flatMap {
          case (row, _) => row.slice(3 * segY, 3 * segY + 3)
        }
      }

      val res = (1 to 9).diff(segValues.filter(_ != 0))
      res
    }
  }

  case class SudokuPuzzle() extends BacktrackDFS[Board, Board] {
    private var found = false

    override def isSolution(result: Board, index: Int, input: Board): Boolean = {
      val left = result.size() - result.moves.size
      found = found || left == 0
      left == 0
    }

    def findSolution(n: Board): Unit = {
      solve(input = n, result = n, index = 0)
    }

    override def shouldContinue(): Boolean = !found

    override def computeCandidates(input: Board, result: Board, index: Int): Seq[Board] = {
      println("Result: ")
      println(result)

      val square = mostConstrainedNext(result)

      square.map { case (x, y) =>
        println(s"Next point: ($x, $y)")
        val nextBoard = result.copy(moves = result.moves :+ Square(x, y, 0))
        val freeValues = Board.freeValues(x, y, nextBoard.cells())
        println(s"Free values for ($x, $y) are: $freeValues")
        freeValues.map { nextValue =>
          val next = nextBoard.moves.last.copy(value = nextValue)
          nextBoard.copy(moves = nextBoard.moves.init :+ next)
        }
      }.getOrElse {
        println("WRONG SOLUTION, Let's get to prev!")
        Seq.empty
      }
    }

//    private def randomNext(input: Board): Option[(Int, Int)] = for {
//      (row, index) <- input.cells().zipWithIndex.find(_._1.exists(_ == 0))
//      point = (index, row.indexOf(0))
//    } yield point

    private def mostConstrainedNext(input: Board): Option[(Int, Int)] = {
      val cells = input.cells()

      val seq = for {
        x <- 0 to 8
        y <- 0 to 8
        (freeX, freeY) = (x, y) if cells(x)(y) == 0
      } yield (freeX, freeY, Board.freeValues(x, y, cells))

      val (x, y, freeValues) = seq.minBy(_._3.size)
      freeValues match {
        case Nil => None
        case _ => Some((x, y))
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

object SudokuMain extends App {
  val board = Board(
    moves = Seq(
      Square(0, 1, 2), Square(0, 3, 6), Square(0, 5, 8),
      Square(1, 0, 5), Square(1, 1, 8), Square(1, 5, 9), Square(1, 6 ,7),
      Square(2, 4, 4),
      Square(3, 0, 3), Square(3, 1, 7), Square(3, 6, 5),
      Square(4, 0, 6), Square(4, 8, 4),
      Square(5, 2, 8), Square(5, 7, 1), Square(5, 8, 3),
      Square(6, 4, 2),
      Square(7, 2, 9), Square(7, 3, 8), Square(7, 7, 3), Square(7, 8, 6),
      Square(8, 3, 3), Square(8, 5, 6), Square(8, 7, 9)
    )
  )

  println("Starting with: ")
  println(board.toString)

  SudokuPuzzle().findSolution(board)
}