package com.example.algorithms.combinatorial

import com.example.algorithms.combinatorial.HeuristicSearchMethods.RandomSampling
import com.example.algorithms.combinatorial.TravelSalesmanProblem.TspInstance

object HeuristicSearchMethods {
  trait RandomSampling[T] {

    def makeRandomSolution(): T

    def solutionCost(rndSolution: T): Double

    def solveWithSampling(samplesCount: Int, bestSolution: T): T = {
      val (result, _) = 1.to(samplesCount).foldLeft(bestSolution -> solutionCost(bestSolution)) {
        case ((best, bestCost), _) =>
          val rndSolution = makeRandomSolution()
          val cost = solutionCost(rndSolution)

          if (cost < bestCost) rndSolution -> cost else best -> bestCost
      }

      result
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
  }.solveWithSampling(2000000, problem)

  println("Ended up with")
  println(solution)
}
