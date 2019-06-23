package org.codingteam.icfpc2019

import main.scala.org.codingteam.icfpc2019.Board

import scala.io.StdIn
import scala.util.Try

class Debugger(task: Task, solution: Solution) {
  private var currentStep = 0
  private var states = Vector(Board(task))
  private def currentState = states(currentStep)

  private def goToStep(stepIndex: Int): Unit = {
    if (stepIndex < 0 || stepIndex > solution.reversedActions.size + 1) {
      println(s"Cannot visit step $stepIndex: solution ends here")
      return
    }

    if (states.size <= stepIndex) {
      val actions = solution.reversedActions.reverse
      var state = currentState
      for (i <- currentStep until stepIndex) {
        val currentAction = actions(i)
        state = currentAction(state)
        states :+= state
      }
    }

    currentStep = stepIndex
  }

  def skipSteps(steps: Int): Unit = {
    goToStep(currentStep + steps)
  }

  def startInteractive(): Unit = {
    var command = ""
    while (command != "x") {
      command match {
        case "x" => return
        case "n" => goToStep(currentStep + 1)
        case "p" => goToStep(currentStep - 1)
        case number if Try(number.toInt).isSuccess => goToStep(number.toInt)
        case _ =>
      }
      printState(currentState)
      println("====\nDebugger controls: n - next; p - prev; x - exit")
      println("[number] - go to step [number]")
      command = StdIn.readLine()
    }
  }

  private def printState(board: Board): Unit = {
    println(s"STEP: $currentStep\n====")
    println(board)
  }
}
