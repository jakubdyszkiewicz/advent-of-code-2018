package day13

import java.util.UUID

import scala.io.Source

/**
  * Worst code so far IMHO. Sadly, no time for refactor :(
  */
object MineCartMadness {

  sealed class DirectionChange

  object GoStraight extends DirectionChange

  object GoLeft extends DirectionChange

  object GoRight extends DirectionChange

  sealed abstract class Direction(val deltaX: Int, val deltaY: Int) {
    def turn(direction: DirectionChange): Direction
  }

  object Left extends Direction(-1, 0) {
    def turn(direction: DirectionChange): Direction = direction match {
      case GoLeft => Down
      case GoRight => Up
      case GoStraight => Left
    }
  }

  object Right extends Direction(1, 0) {
    def turn(direction: DirectionChange): Direction = direction match {
      case GoLeft => Up
      case GoRight => Down
      case GoStraight => Right
    }
  }

  object Up extends Direction(0, -1) {
    def turn(direction: DirectionChange): Direction = direction match {
      case GoLeft => Left
      case GoRight => Right
      case GoStraight => Up
    }
  }

  object Down extends Direction(0, 1) {
    def turn(direction: DirectionChange): Direction = direction match {
      case GoLeft => Right
      case GoRight => Left
      case GoStraight => Down
    }
  }

  case class Cart(
    x: Int,
    y: Int,
    direction: Direction,
    crossChange: DirectionChange = GoLeft,
    id: String = UUID.randomUUID().toString) extends Ordered[Cart] {

    def move: Cart = copy(x + direction.deltaX, y + direction.deltaY)

    def changeDirectionAtCross: Cart = {
      val nextDirection = direction.turn(crossChange)
      val nextCrossChange = crossChange match {
        case GoLeft => GoStraight
        case GoStraight => GoRight
        case GoRight => GoLeft
      }
      copy(direction = nextDirection, crossChange = nextCrossChange)
    }

    import scala.math.Ordered.orderingToOrdered

    def compare(that: Cart): Int = (this.y, this.x) compare(that.y, that.x)
  }

  type Field = Array[Array[Char]]

  def parseFieldsAndCarts(lines: Seq[String]): (Field, List[Cart]) = {
    val field = Array.ofDim[Char](lines.length, lines.maxBy(_.length).length)
    var carts = List[Cart]()
    for (i <- lines.indices) {
      for (j <- lines(i).indices) {
        lines(i)(j) match {
          case '<' =>
            carts ::= Cart(j, i, Left)
            field(i)(j) = '-'
          case '>' =>
            carts ::= Cart(j, i, Right)
            field(i)(j) = '-'
          case 'v' =>
            carts ::= Cart(j, i, Down)
            field(i)(j) = '-'
          case '^' =>
            carts ::= Cart(j, i, Up)
            field(i)(j) = '-'
          case x: Char => field(i)(j) = x
        }
      }
    }
    (field, carts)
  }

  def tick(field: Field, cart: Cart): Cart = {
    val moved = cart.move
    (field(moved.y)(moved.x), moved.direction) match {
      case ('/', Right) => moved.copy(direction = Up)
      case ('/', Down) => moved.copy(direction = Left)
      case ('/', Left) => moved.copy(direction = Down)
      case ('/', Up) => moved.copy(direction = Right)
      case ('\\', Left) => moved.copy(direction = Up)
      case ('\\', Up) => moved.copy(direction = Left)
      case ('\\', Right) => moved.copy(direction = Down)
      case ('\\', Down) => moved.copy(direction = Right)
      case ('+', _) => moved.changeDirectionAtCross
      case ('-', _) => moved
      case ('|', _) => moved
      case _ => throw new IllegalStateException("Cart of the track!")
    }
  }

  def tickUntilCollision(field: Field, carts: List[Cart]): (Int, Int) = {
    println(carts)
    var sortedCarts = carts.sorted
    for (i <- carts.indices) {
      sortedCarts = sortedCarts.updated(i, tick(field, sortedCarts(i)))
      val collisions = sortedCarts.groupBy(cart => (cart.x, cart.y))
        .find { case ((x, y), carts) => carts.length > 1 }
      if (collisions.isDefined) {
        return collisions.get._1
      }
    }
    tickUntilCollision(field, sortedCarts)
  }

  def tickUntilCollisionRemovingCarts(field: Field, carts: Seq[Cart]): Cart = {
    if (carts.length == 1) {
      return carts.head
    }
    val sortedCarts = carts.sorted.toArray
    var toRemoveIds = List[String]()
    for (i <- sortedCarts.indices) {
      if (!toRemoveIds.contains(sortedCarts(i).id)) {
        val moved = tick(field, sortedCarts(i))
        val collision = sortedCarts
          .filter(c => !toRemoveIds.contains(c.id))
          .filter(c => c.id != moved.id)
          .find(c => c.x == moved.x && c.y == moved.y)
        if (collision.isDefined) {
          toRemoveIds :::= List(moved.id, collision.get.id)
        }
        sortedCarts(i) = moved
      }
    }
    tickUntilCollisionRemovingCarts(field, sortedCarts.filter(c => !toRemoveIds.contains(c.id)))
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day13/input.txt").getLines.toSeq
    val (field, carts) = parseFieldsAndCarts(input)
    for (i <- field.indices) {
      for (j <- field(i).indices) {
        print(field(i)(j))
      }
      println()
    }
    println(tickUntilCollision(field, carts))
    println(tickUntilCollisionRemovingCarts(field, carts))
  }
}
