package com.example.algorithms.combinatorial

import com.example.algorithms.combinatorial.HeuristicSearchMethods.{LocalSearch, RandomSampling}
import com.example.algorithms.combinatorial.TravelSalesmanProblem.{Point, TspInstance}

import scala.annotation.tailrec

object HeuristicSearchMethods {
  trait RandomSampling[T] {
    def makeRandomSolution(): T

    def solutionCost(rndSolution: T): Double

    def solve(samplesCount: Int, bestSolution: T): T = {
      val (result, _) = 1.to(samplesCount).foldLeft(bestSolution -> solutionCost(bestSolution)) {
        case ((best, bestCost), _) =>
          val rndSolution = makeRandomSolution()
          val cost = solutionCost(rndSolution)

          if (cost < bestCost) rndSolution -> cost else best -> bestCost
      }

      result
    }
  }

  trait LocalSearch[T, P] {
    case class SearchStep(stuck: Boolean, solution: T, cost: Double)

    def makeRandomSolution(): T

    def solutionCost(rndSolution: T): Double

    def elements(solution: T): Seq[P]

    def swapElements(solution: T, i: Int, j: Int): T

    def solve(): T = {
      val rnd = makeRandomSolution()
      doSolve(SearchStep(stuck = false, rnd, solutionCost(rnd)))
    }

    @tailrec
    private def doSolve(searchStep: SearchStep): T = {
      if (searchStep.stuck)
        searchStep.solution
      else {
        val size = elements(searchStep.solution).size
        val swaps: Seq[(Int, Int)] = 1.until(size).flatMap {
          i => i.until(size).map(j => (i, j))
        }
        val nextStep = swaps.foldLeft(searchStep.copy(stuck = true)) {
          case (step @ SearchStep(_, solution, cost), (i, j)) =>
            val newSolution = swapElements(solution, i, j)
            val delta = costOfSwapping(solution, newSolution)
            if (delta < 0)
              SearchStep(stuck = false, newSolution, cost + delta)
            else
              step
        }

        doSolve(nextStep)
      }
    }

    private def costOfSwapping(was: T, now: T): Double = {
      // could be implemented more effectively if we calculate was/willbe distances between swapped points
      // but I don't care
      solutionCost(now) - solutionCost(was)
    }
  }
}

object TspRandomSampled extends App {
  val problem = TravelSalesmanProblem.read()
  println("Started with")
  println(problem)

  val solution: TspInstance = new RandomSampling[TspInstance] {
    override def makeRandomSolution(): TspInstance = {
      TspInstance(problem.points.head +: scala.util.Random.shuffle(problem.points.tail))
    }

    override def solutionCost(rndSolution: TspInstance): Double = rndSolution.cost()
  }.solve(2000000, problem)

  println("Ended up with")
  println(solution)
}

object TspLocalSearch extends App {
  val problem = TravelSalesmanProblem.read()
  println("Started with")
  println(problem)

  val result = 1.to(1000).map { _ =>
    val solution = new LocalSearch[TspInstance, Point] {
      override def makeRandomSolution(): TspInstance = {
        TspInstance(problem.points.head +: scala.util.Random.shuffle(problem.points.tail))
      }

      override def solutionCost(rndSolution: TspInstance): Double = rndSolution.cost()

      override def elements(solution: TspInstance): Seq[Point] = solution.points

      override def swapElements(solution: TspInstance, i: Int, j: Int): TspInstance = {
        val list = solution.points
        solution.copy(points = list.updated(i, list(j)).updated(j, list(i)))
      }
    }.solve()

    solution
  }.minBy(_.cost())


  println("Ended up with")
  println(result)
}
