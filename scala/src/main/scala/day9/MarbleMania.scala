package day9

import scala.collection.mutable

object MarbleMania {

  def highScore(nPlayers: Int, pointsLimit: Int): Int = {
    var scores = List.fill(nPlayers)(0)
    var marbles = List(0, 1)
    var curMarbleIndex = 1
    var curMarble = marbles(curMarbleIndex)
    var curPlayer = 0
    while (curMarble < pointsLimit) {
      if (curMarble % 1000 == 0) {
        println(curMarble)
      }
      val nextMarble = curMarble + 1
      if (nextMarble % 23 == 0) {
        var marbleToRemoveIndex = curMarbleIndex - 7
        if (marbleToRemoveIndex < 0) {
          marbleToRemoveIndex += marbles.size
        }
        scores = scores.updated(curPlayer, scores(curPlayer) + nextMarble + marbles(marbleToRemoveIndex))
        marbles = removeAtIndex(marbles, marbleToRemoveIndex)

        val nextMarbleIndex = marbleToRemoveIndex % marbles.size

        curMarble = nextMarble
        curMarbleIndex = nextMarbleIndex
      } else {
        var nextMarbleIndex = (curMarbleIndex + 2) % marbles.size
        if (nextMarbleIndex == 0) {
          nextMarbleIndex = marbles.size
        }

        marbles = insert(marbles, nextMarbleIndex, nextMarble)
        curMarble = nextMarble
        curMarbleIndex = nextMarbleIndex
        curPlayer = (curPlayer + 1) % nPlayers
      }
    }
    scores.max
  }

  def insert[T](list: List[T], i: Int, value: T): List[T] = {
    val (front, back) = list.splitAt(i)
    front ++ List(value) ++ back
  }

  def removeAtIndex[T](list: List[T], idx: Int): List[T] = {
    list.take(idx) ++ list.drop(idx + 1)
  }

  def main(args: Array[String]): Unit = {
//    println(highScore(nPlayers = 419, pointsLimit = 71052))
    println(highScore(nPlayers = 419, pointsLimit = 71052 * 100))
  }
}
