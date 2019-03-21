package com.example.algorithms.combinatorial

import com.example.algorithms.combinatorial.HeuristicSearchMethods.RandomSampling
import com.example.algorithms.combinatorial.TravelSalesmanProblem.TspInstance

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

    def swapElements(solution: T, i: Int, j: Int): (Double, T) = ???

    @tailrec
    private def doSolve(searchStep: SearchStep): T = {
      if (searchStep.stuck)
        searchStep.solution
      else {
        val size = elements(searchStep.solution).size
        val swaps: Seq[(Int, Int)] = 0.to(size).flatMap {
          i => i.to(size).map(j => (i, j))
        }
        val nextStep = swaps.foldLeft(searchStep.copy(stuck = true)) {
          case (step @ SearchStep(_, solution, cost), (i, j)) =>
            val (delta, improvedSolution) = swapElements(solution, i, j)
            if (delta < 0)
              SearchStep(stuck = false, improvedSolution, cost + delta)
            else
              step
        }

        doSolve(nextStep)
      }
    }

    def solve(): T = {
      val rnd = makeRandomSolution()
      doSolve(SearchStep(stuck = false, rnd, solutionCost(rnd)))
    }
  }
}

object TspRandomSampled extends App {
  val problem = TravelSalesmanProblem.read()
  println("Started with")
  println(problem)

  val solution: TspInstance = new RandomSampling[TspInstance] {
    override def makeRandomSolution(): TspInstance = {
      TspInstance(scala.util.Random.shuffle(problem.points))
    }

    override def solutionCost(rndSolution: TspInstance): Double = rndSolution.cost()
  }.solve(2000000, problem)

  println("Ended up with")
  println(solution)
}
