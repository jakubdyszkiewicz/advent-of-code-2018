package day11

import commons.Crossable.Crossable

object ChronalCharge {

  type Matrix = Array[Array[Int]]

  private def createArray(serialNum: Int): Matrix = {
    val matrix = Array.ofDim[Int](301, 301)
    for (x <- 1 to 300) {
      for (y <- 1 to 300) {
        val rackId = x + 10
        var powerLevel = rackId * y
        powerLevel += serialNum
        powerLevel *= rackId
        powerLevel = (powerLevel / 100) % 10
        powerLevel -= 5
        matrix(x)(y) = powerLevel
      }
    }
    matrix
  }

  private def prefixSumMatrix(matrix: Matrix): Matrix = {
    val sumMatrix = Array.ofDim[Int](301, 301)
    for (x <- 1 to 300) {
      for (y <- 1 to 300) {
        sumMatrix(x)(y) = sumMatrix(x - 1)(y) + sumMatrix(x)(y - 1) - sumMatrix(x - 1)(y - 1) + matrix(x)(y)
      }
    }
    sumMatrix
  }

  case class Square(topLeftX: Int, topLeftY: Int, sum: Int)

  private def findMaxSumSquare(prefixSumMatrix: Matrix, squareSize: Int): Square =
    (squareSize to 300)
      .cross(squareSize to 300)
      .map { case(x, y) =>
        val sum = prefixSumMatrix(x)(y) -
          prefixSumMatrix(x - squareSize)(y) -
          prefixSumMatrix(x)(y - squareSize) +
          prefixSumMatrix(x - squareSize)(y - squareSize)
        Square(
          topLeftX = x - squareSize + 1,
          topLeftY = y - squareSize + 1,
          sum = sum)
      }
      .maxBy(square => square.sum)

  def solve1(serialNum: Int): Square = {
    val matrix = createArray(serialNum)
    val prefSumMatrix = prefixSumMatrix(matrix)
    findMaxSumSquare(prefSumMatrix, squareSize = 3)
  }

  def solve2(serialNum: Int): (Square, Int) = {
    val matrix = createArray(serialNum)
    val prefSumMatrix = prefixSumMatrix(matrix)
    (1 to 300)
      .map(squareSize => findMaxSumSquare(prefSumMatrix, squareSize) -> squareSize)
      .maxBy { case(square, squareSize) => square.sum }
  }

  def main(args: Array[String]): Unit = {
    println(solve1(9810))
    println(solve2(9810))
  }
}
