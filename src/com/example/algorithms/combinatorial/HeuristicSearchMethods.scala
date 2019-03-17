package com.example.algorithms.combinatorial

object HeuristicSearchMethods {

  case class RandomSampling[T]() {

    def makeRandomSolution(): T = ???

    def solutionCost(rndSolution: T): Double = ???

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
