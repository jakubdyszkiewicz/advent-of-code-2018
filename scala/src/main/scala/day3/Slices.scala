import scala.io.Source

object Slices {

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]): Traversable[(X, Y)] = for { x <- xs; y <- ys } yield (x, y)
  }

  case class Claim(id: Int, startX: Int, startY: Int, width: Int, height: Int) {
    def size: Int = width * height
  }

  type Coordinates = (Int, Int)

  class Area(val fields: Map[Coordinates, Set[Claim]] = Map()) {

    def claim(claim: Claim): Area = {
      val claims = (claim.startX until claim.startX + claim.width)
        .cross(claim.startY until claim.startY + claim.height)
        .map(coords => coords -> (fields.getOrElse(coords, Set()) + claim))
        .toMap
      new Area(fields ++ claims)
    }

    def multipleClaimFields: Map[Coordinates, Set[Claim]] =
      fields.filter { case(_, claims) => claims.size > 1 }

    def idOfNotClaimedByAnyoneElse: Option[Int] =
      fields.filter { case(_, claims) => claims.size == 1 }
        .groupBy { case(_, claims) => claims.head }
        .find { case(coords, claims) => coords.size == claims.size }
        .map { case(claim, _) => claim.id }

  }

  private val regex = """#(\d{1,}) @ (\d{1,}),(\d{1,}): (\d{1,})x(\d{1,})+""".r
  def parseClaim(line: String): Claim = line match {
    case regex(id, startX, startY, width, height) =>
      Claim(id.toInt, startX.toInt, startY.toInt, width.toInt, height.toInt)
  }

  def main(args: Array[String]): Unit = {
    val claims = Source.fromResource("day3/input.txt").getLines
      .map(parseClaim)
      .toList
    val area = claims.foldLeft(new Area()) { (area, claim) => area.claim(claim) }
    println(area.multipleClaimFields.size)
    println(area.idOfNotClaimedByAnyoneElse)
  }
}
