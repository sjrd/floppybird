/*
Copyright 2014 Nebez Briefkani
floppybird - main.js
(translated to Scala.js by Sébastien Doeraene)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package be.doeraene.floppybird

import scala.collection.mutable

import scala.scalajs.js
import js.annotation.JSExport
import js.Dynamic.{global => g, newInstance => dynNew, literal => lit}

import org.scalajs.dom
import org.scalajs.jquery.{jQuery => jQ, JQuery}

sealed abstract class State
object State {
  case object SplashScreen extends State
  case object GameScreen extends State
  case object ScoreScreen extends State
}

object Config {
  val DebugMode = dom.location.search.contains("?debug")
  val EasyMode = dom.location.search.contains("?easy")

  val Gravity = 0.25
  val PipeHeight = if (EasyMode) 200 else 90
  val PipeWidth = 52
}

object Sounds {
  val Volume = 30

  val Jump = load("sfx_wing.ogg")
  val Score = load("sfx_point.ogg")
  val Hit = load("sfx_hit.ogg")
  val Die = load("sfx_die.ogg")
  val Swoosh = load("sfx_swooshing.ogg")

  g.buzz.all().setVolume(Volume)

  private def load(name: String) =
    dynNew(g.buzz.sound)(s"assets/sounds/$name")
}

object Bird{
  var velocity: Double = 0
  var position: Double = 180
  var rotation: Double = 0
  var jump: Double = -4.6
}

trait JQueryOffset extends js.Object {
  val left: js.Number
  val top: js.Number
  val width: js.Number
  val height: js.Number
}

object TypingFixes {
  implicit class JQueryFixes(val self: JQuery) extends AnyVal {
    def offst(): JQueryOffset =
      self.asInstanceOf[js.Dynamic].offset().asInstanceOf[JQueryOffset]

    def pos(): JQueryOffset =
      self.asInstanceOf[js.Dynamic].position().asInstanceOf[JQueryOffset]
  }
}

@JSExport
object FloppyBird {
  import Config._
  import Bird._
  import TypingFixes._

  var currentState: State = State.SplashScreen
  var score: Int = 0
  var highScore: Int = 0
  val pipes = mutable.ArrayBuffer.empty[JQuery]
  var replayClickable: Boolean = false

  // loops (handles to setInterval)
  var loopGameloop: js.Number = null
  var loopPipeloop: js.Number = null

  @JSExport
  def main(): Unit = {
    jQ(dom.document).ready { () =>
      // Get the highscore
      for (savedScore <- getCookie("highscore").map(js.parseInt(_).toInt))
        highScore = savedScore

      // Start with the splash screen
      showSplash()
    }

    // Handle space bar
    jQ(dom.document).keydown { (e0: dom.Event) =>
      val e = e0.asInstanceOf[dom.KeyboardEvent]
      if (e.keyCode == (32: js.Number)) { // space bar
        // In ScoreScreen, hitting space should click the "replay" button.
        // Otherwise it's just a regular space bar
        currentState match {
          case State.ScoreScreen => jQ("#replay").click()
          case _ => screenClick()
        }
      }
    }

    // Handle mouse down OR touch start
    if (js.Object.hasProperty(dom.window, "ontouchstart"))
      jQ(dom.document).on("touchstart", screenClick _)
    else
      jQ(dom.document).on("mousedown", screenClick _)


    jQ("#replay").click(clickReplay _)
  }

  private def getCookie(cname: String) = {
    val name = s"$cname="
    dom.document.cookie.toString().split(";") collectFirst {
      case elem if elem.trim().startsWith(name) =>
        elem.trim().stripPrefix(name)
    }
  }

  private def setCookie(cname: String, cvalue: String, exdays: Int) = {
    val now = new js.Date()
    val d = new js.Date(now.getTime() + (exdays*24*60*60*1000))
    g.document.cookie = s"$cname=$cvalue; expires=${d.toUTCString()}"
  }

  private def showSplash() = {
    currentState = State.SplashScreen

    // Set the defaults (again)
    velocity = 0
    position = 180
    rotation = 0
    score = 0

    // Update the player in preparation for the next game
    jQ("#player").css(lit(y = 0, x = 0))
    updatePlayer(jQ("#player"))

    Sounds.Swoosh.stop()
    Sounds.Swoosh.play()

    // Clear out all the pipes if there are any
    jQ(".pipe").remove()
    pipes.clear()

    // Make everything animated again
    jQ(".animated").css("animation-play-state", "running")
    jQ(".animated").css("-webkit-animation-play-state", "running")

    // Fade in the splash
    jQ("#splash").asInstanceOf[js.Dynamic].transition(
        lit(opacity = 1), 2000, "ease")
  }

  private def startGame() = {
    currentState = State.GameScreen

    // Fade out the splash
    jQ("#splash").stop()
    jQ("#splash").asInstanceOf[js.Dynamic].transition(
        lit(opacity = 0), 500, "ease")

    // Update the big score
    setBigScore()

    // Show the bounding boxes in debug mode
    if (DebugMode)
      jQ(".boundingbox").show()

    // Start up our loops
    val updateRate = 1000.0 / 60.0 // 60 times a second
    loopGameloop = dom.setInterval(gameLoop _, updateRate)
    loopPipeloop = dom.setInterval(updatePipes _, 1400)

    // Jump from the start!
    playerJump()
  }

  private def updatePlayer(player: JQuery) = {
    // Rotation
    rotation = Math.min((velocity / 10) * 90, 90)

    // Apply rotation and position
    player.css(lit(rotate = rotation, top = position))
  }

  private def gameLoop(): Unit = {
    val player = jQ("#player")

    // Update the player speed/position
    velocity += Gravity
    position += velocity

    // Update the player
    updatePlayer(player)

    // Create the bounding box
    val box = dom.document.getElementById("player").getBoundingClientRect()
    val origWidth = 34.0
    val origHeight = 24.0

    val boxWidth = origWidth - (Math.sin(Math.abs(rotation) / 90) * 8)
    val boxHeight = (origHeight + box.height) / 2
    val boxLeft = ((box.width - boxWidth) / 2) + box.left
    val boxTop = ((box.height - boxHeight) / 2) + box.top
    val boxRight = boxLeft + boxWidth
    val boxBottom = boxTop + boxHeight

    // If we are in debug mode, draw the bounding box
    if (DebugMode) {
      val boundingBox = jQ("#playerbox")
      boundingBox.css(lit(
          left = boxLeft, top = boxTop, height = boxHeight, width = boxWidth))
    }

    // Did we hit the ground
    if (box.bottom >= jQ("#land").offst().top)
      return playerDead()

    // We can't go any further without a pipe
    if (pipes.isEmpty)
      return

    // Determine the bounding box of the next pipes inner area
    val nextPipe = pipes.head
    val nextPipeUpper = nextPipe.children(".pipe_upper")

    val pipeTop = nextPipeUpper.offst().top + nextPipeUpper.height()
    // for some reason it starts at the inner pipes offset, not the outer pipes.
    val pipeLeft = nextPipeUpper.offst().left - 2
    val pipeRight = pipeLeft + PipeWidth
    val pipeBottom = pipeTop + PipeHeight

    if (DebugMode) {
      val boundingBox = jQ("#pipebox")
      boundingBox.css(lit(
          left = pipeLeft, top = pipeTop, height = PipeHeight, width = PipeWidth))
    }

    // Have we gotten inside the pipe yet?
    if (boxRight > pipeLeft) {
      // We're within the pipe, have we passed between upper and lower pipes?
      if (boxTop > pipeTop && boxBottom < pipeBottom) {
        // yead! we're within the bounds
      } else {
        // no! we touched the pipe
        return playerDead()
      }
    }

    // Have we passed the imminent danger?
    if (boxLeft > pipeRight) {
      // yes, remove it
      pipes.remove(0)
      // and score a point
      playerScore()
    }
  }

  private def screenClick() = {
    currentState match {
      case State.GameScreen   => playerJump()
      case State.SplashScreen => startGame()
      case _ =>
    }
  }

  private def playerJump() = {
    velocity = jump
    // play jump sound
    Sounds.Jump.stop()
    Sounds.Jump.play()
  }

  private def setBigScore(erase: Boolean = false) =
    setScoreGeneric(score, "#bigscore", "big", erase)
  private def setSmallScore(erase: Boolean = false) =
    setScoreGeneric(score, "#currentscore", "small", erase)
  private def setHighScore(erase: Boolean = false) =
    setScoreGeneric(highScore, "#highscore", "small", erase)

  private def setScoreGeneric(score: Int,
      elemID: String, size: String, erase: Boolean) = {
    val elemScore = jQ(elemID)
    elemScore.empty()

    if (!erase) {
      for (digit <- score.toString())
        elemScore.append(
            s"""<img src='assets/font_${size}_$digit.png' alt='$digit'>""")
    }
  }

  /** Sets the medal, and returns true if there was any. */
  private def setMedal(): Boolean = {
    val elemMedal = jQ("#medal")
    elemMedal.empty()

    if (score < 10) {
      false
    } else {
      val medal =
        if (score >= 40) "platinum"
        else if (score >= 30) "gold"
        else if (score >= 20) "silver"
        else "bronze"

      elemMedal.append(
          s"""<img src='assets/medal_$medal.png' alt='$medal'>""")
      true
    }
  }

  private def playerDead() = {
    // Stop animating everything
    jQ(".animated").css("animation-play-state", "pause")
    jQ(".animated").css("-webkit-animation-play-state", "pause")

    // Drop the bird to the floor
    val playerBottom = jQ("#player").pos().top + jQ("#player").width() // we use width because he'll be rotated 90 deg
    val floor = jQ("#flyarea").height()
    val movey = Math.max(0, floor - playerBottom)
    jQ("#player").asInstanceOf[js.Dynamic].transition(
        lit(y = movey + "px", rotate = 90), 1000, "easeInOutCubic")

    // It's time to change states.
    // As of now we're considered ScoreScreen to disable left click/flying
    currentState = State.ScoreScreen

    // Destroy our game loops
    dom.clearInterval(loopGameloop)
    dom.clearInterval(loopPipeloop)
    loopGameloop = null
    loopPipeloop = null

    // Mobile browsers don't support buzz bindOnce event
    // TODO
    showScore()
  }

  private def showScore() = {
    // Unhide us
    jQ("#scoreboard").css("display", "block")

    // Remove the big score
    setBigScore(erase = true)

    // Have they beaten their high score?
    if (score > highScore) {
      // yeah!
      highScore = score
      // save it
      setCookie("highscore", highScore.toString(), 999)
    }

    // Update the scoreboard
    setSmallScore()
    setHighScore()
    val wonMedal = setMedal()

    // Swoosh!
    Sounds.Swoosh.stop()
    Sounds.Swoosh.play()

    // Show the scoreboard
    jQ("#scoreboard").css(lit(y = "40px", opacity = 0)) // move it down so we can slide it up
    jQ("#replay").css(lit(y = "40px", opacity = 0))
    jQ("#scoreboard").asInstanceOf[js.Dynamic].transition(
        lit(y = "0px", opacity = 1), 600, "ease", { () =>
          // When the animation is done, animate in the replay button and SWOOSH!
          Sounds.Swoosh.stop()
          Sounds.Swoosh.play()
          jQ("#replay").asInstanceOf[js.Dynamic].transition(
              lit(y = "0px", opacity = 1), 600, "ease")

          // Also animate in the Medla! Woo!
          if (wonMedal) {
            jQ("#medal").css(lit(scale = 2, opacity = 0))
            jQ("#medal").asInstanceOf[js.Dynamic].transition(
                lit(opacity = 1, scale = 1), 1200, "ease")
          }
        })

    // Make the replay button clickable
    replayClickable = true
  }

  private def clickReplay() = if (replayClickable) {
    replayClickable = false

    // SWOOSH!
    Sounds.Swoosh.stop()
    Sounds.Swoosh.play()

    // Fade out the scoreboard
    jQ("#scoreboard").asInstanceOf[js.Dynamic].transition(
        lit(y = "-40px", opacity = 0), 1000, "ease", { () =>
          // When that's done, display us back to nothing
          jQ("#scoreboard").css("display", "none")

          // Start the game over
          showSplash()
        })
  }

  private def playerScore() = {
    score += 1
    // Play score sound
    Sounds.Score.stop()
    Sounds.Score.play()
    setBigScore()
  }

  private def updatePipes() = {
    // Do any pipes need removal
    jQ(".pipe").filter({ (pipe: dom.HTMLElement) =>
      jQ(pipe).pos().left.toDouble <= -100
    }: js.ThisFunction).remove()

    // Add a new pipe (top height + bottom height + pipeheight == 420)
    // and put it in our tracker
    val padding = 80
    val constraint = 420 - PipeHeight - (padding*2) // double padding (for top and bottom)
    val topHeight = Math.floor((Math.random()*constraint) + padding) // add lower padding
    val bottomHeight = (420 - PipeHeight) - topHeight
    val newPipe = jQ(
        s"""<div class="pipe animated">
          <div class="pipe_upper" style="height: ${topHeight}px;"></div>
          <div class="pipe_lower" style="height: ${bottomHeight}px;"></div>
        </div>""")
    jQ("#flyarea").append(newPipe)
    pipes += newPipe
  }
}
