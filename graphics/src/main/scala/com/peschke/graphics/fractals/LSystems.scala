package com.peschke.graphics.fractals

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.paint.Color
import scalafx.scene.{Group, Scene}
import scalafx.scene.canvas.{ Canvas, GraphicsContext }
import javafx.application.Platform

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.global

import akka.actor.ActorSystem
import akka.actor.Props

import com.peschke.math.l_system.SquareKochCurve
import com.peschke.graphics.utils.Timer
import com.peschke.graphics.utils.Turtle

object LSystems extends JFXApp {
  val actorSystem = ActorSystem("TimerSystem", defaultExecutionContext = Some(global))

  val canvas = new Canvas(600,600)
  val gc = canvas.graphicsContext2D

  val rootPane = new Group
  rootPane.children = List(canvas)

  stage = new PrimaryStage {
    scene = new Scene {
      root = rootPane
    }
  }

  gc.lineWidth = 1

  val startPoint = Turtle(20,580,0)

  val delayPerGeneration = (0 to 6)
    .map(generation => generation -> (.25 / Math.pow(5, generation)).seconds)
    .toMap

  val lineLength = (0 to 6)
    .map(generation => generation -> (600 - 40) / Math.pow(3, generation))
    .toMap

  val lineColor = (0 to 6)
    .map(generation => generation -> Color.Black.opacity(1.0 / 6.0 * generation))
    .toMap

  case class KochCurveContext(generation: Int,
                              turtle: Turtle,
                              instructions: Iterator[SquareKochCurve]) {
    def delay: Option[FiniteDuration] = Some(delayPerGeneration(generation))

    def drawSegment: Timer.Action[KochCurveContext] = {
      if (!instructions.hasNext) {
        val nextGeneration = (generation + 1) % 5
        if (nextGeneration == 0) {
          gc.clearRect(0,0,600,600)
        }
        Timer.Action.Next(KochCurveContext(nextGeneration))
      }
      else {
        instructions.next match {
          case SquareKochCurve.L => Timer.Action.Next(copy(turtle = turtle.rotate(-90)), delay)
          case SquareKochCurve.R => Timer.Action.Next(copy(turtle = turtle.rotate(90)), delay)
          case SquareKochCurve.F =>
            val distance = lineLength(generation)
            Platform.runLater(new Runnable {
              def run(): Unit = {
                gc.stroke = lineColor(generation)
                turtle.line(distance)(gc)
              }
            })
            Timer.Action.Next(
              copy(turtle = turtle.move(distance)),
              if (instructions.hasNext) delay else Some(1.second))
        }
      }
    }
  }

  object KochCurveContext extends (Int => KochCurveContext){
    def apply(generation: Int): KochCurveContext = {
      KochCurveContext(
        generation,
        startPoint,
        SquareKochCurve.generation(generation))
    }
  }

  val timer = actorSystem.actorOf(Props(new Timer[KochCurveContext](
    0.5.seconds,
    KochCurveContext(0),
    _.drawSegment
  )))

  timer ! Timer.Msg.Start
}
