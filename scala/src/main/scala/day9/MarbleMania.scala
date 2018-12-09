package day9

import java.util

object MarbleMania {

  class CyclicIterator[T](list: util.List[T]) extends util.ListIterator[T] {
    private var curIterator = list.listIterator()

    override def hasNext: Boolean = curIterator.hasNext || curIterator.hasPrevious

    override def next(): T = {
      if (!curIterator.hasNext) {
        curIterator = list.listIterator()
      }
      curIterator.next
    }

    override def hasPrevious: Boolean = !list.isEmpty

    override def previous(): T = {
      if (!curIterator.hasPrevious) {
        curIterator = list.listIterator(list.size)
      }
      curIterator.previous
    }

    override def nextIndex(): Int = ???

    override def previousIndex(): Int = ???

    override def set(e: T): Unit = curIterator.set(e)

    override def add(e: T): Unit = curIterator.add(e)

    def current: T = {
      previous()
      next()
    }

    override def remove(): Unit = curIterator.remove()

    def removeReturning(): T = {
      val value = current
      curIterator.remove()
      value
    }
  }

  def highScore(nPlayers: Int, pointsLimit: Int): Long = {
    var scores = List.fill(nPlayers)(0L)
    val marbles = new CyclicIterator(new util.LinkedList[Int]())
    marbles.add(0)

    (1 until pointsLimit).foreach { nextMarble =>
      val player = nextMarble % nPlayers
      if (nextMarble % 23 == 0) {
        (1 to 7).foreach(_ => marbles.previous())
        scores = scores.updated(player, scores(player) + nextMarble + marbles.removeReturning())
        marbles.next()
      } else {
        marbles.next()
        marbles.add(nextMarble)
      }
    }
    scores.max
  }

  def main(args: Array[String]): Unit = {
    println("Part1 " + highScore(nPlayers = 419, pointsLimit = 71052))
    println("Part2 " + highScore(nPlayers = 419, pointsLimit = 71052 * 100))
  }
}
