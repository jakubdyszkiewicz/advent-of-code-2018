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
        curIterator = list.listIterator(list.size - 1)
      }
      curIterator.previous
    }

    override def nextIndex(): Int = ??? //curIterator.previousIndex()

    override def previousIndex(): Int = ??? //curIterator.nextIndex()

    override def set(e: T): Unit = curIterator.set(e)

    override def add(e: T): Unit = curIterator.add(e)

    def current: T = {
      previous()
      next()
    }

    def removeReturning(): T = {
      val value = current
      curIterator.remove()
      value
    }
  }

  def highScore(nPlayers: Int, pointsLimit: Int): Int = {
    var scores = List.fill(nPlayers)(0)
    val marbles = new CyclicIterator(new util.LinkedList[Int]())
    marbles.add(0)

    var curPlayer = 0
    var nextMarble = 1
    while (nextMarble < pointsLimit) {
      if (nextMarble % 23 == 0) {
        1 to 7 foreach { _ => marbles.previous() }
        scores = scores.updated(curPlayer, scores(curPlayer) + nextMarble + marbles.removeReturning())
      } else {
        marbles.next()
        marbles.add(nextMarble)
      }
      curPlayer = (curPlayer + 1) % nPlayers
      nextMarble += 1
    }
    scores.max
  }

  def main(args: Array[String]): Unit = {
    //    println(highScore(nPlayers = 419, pointsLimit = 71052))
    println(highScore(nPlayers = 419, pointsLimit = 71052 * 100))
  }
}
