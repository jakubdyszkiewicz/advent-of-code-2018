package day10

import scala.io.Source

object StarsAlign {

  case class Star(posX: Int, posY: Int, velocityX: Int, velocityY: Int) {
    def move: Star = this.copy(posX = posX + velocityX, posY = posY + velocityY)
  }

  def moveUntilHeight(stars: Seq[Star], heightLimit: Int, step: Int = 0): (Seq[Star], Int) = {
    val minY = stars.minBy(_.posY).posY
    val maxY = stars.maxBy(_.posY).posY
    val height = maxY - minY
    if (height <= heightLimit) {
      return (stars, step)
    }
    moveUntilHeight(stars.map(_.move), heightLimit, step + 1)
  }

  def showHeaven(stars: Seq[Star]): Unit = {
    val minY = stars.minBy(_.posY).posY
    val maxY = stars.maxBy(_.posY).posY
    val minX = stars.minBy(_.posX).posX
    val maxX = stars.maxBy(_.posX).posX

    (minY to maxY).foreach { y =>
      (minX to maxX).foreach { x =>
        val field = stars.find(star => star.posX == x && star.posY == y)
          .map(_ => '#')
          .getOrElse('.')
        print(field)
      }
      println()
    }
  }

  private val starRegex = raw"position=<[ ]*([-]?)(\d+),[ ]*([-]?)(\d+)> velocity=<[ ]*([-]?)(\d+),[ ]*([-]?)(\d+)>".r

  def parseStar(line: String): Star = {
    def toNumber(sign: String, num: String): Int = (if (sign == "-") -1 else 1) * num.toInt

    line match {
      case starRegex(signPosX, posX, signPosY, posY, signVelX, velX, signVelY, velY) =>
        Star(
          posX = toNumber(signPosX, posX),
          posY = toNumber(signPosY, posY),
          velocityX = toNumber(signVelX, velX),
          velocityY = toNumber(signVelY, velY))
    }
  }

  def main(args: Array[String]): Unit = {
    val stars = Source.fromResource("day10/input.txt").getLines.map(parseStar).toSeq
    val (movedStars, steps) = moveUntilHeight(stars, heightLimit = 9)
    showHeaven(movedStars)
    println(steps)
  }

}
