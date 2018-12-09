package day8

import scala.io.Source

object MemoryManeuver {

  type Metadata = Seq[Int]
  type Input = List[Int]

  def extractMetadata(input: Input): (Input, Metadata) = {
    var nChildren :: nMetadata :: restInput = input
    var metadata: List[Metadata] = List()
    (0 until nChildren).foreach { _ =>
      val (newRestInput, restMetadata) = extractMetadata(restInput)
      metadata = metadata ::: restMetadata :: Nil
      restInput = newRestInput
    }
    val newMetadata = restInput.take(nMetadata) ++ metadata.flatten
    (restInput.drop(nMetadata), newMetadata)
  }

  def part1(input: Input): Int = extractMetadata(input)._2.sum

  def extractIndexedMetadata(input: Input): (Input, Metadata) = {
    var nChildren :: nMetadata :: restInput = input
    var metadata: List[Metadata] = List()
    (0 until nChildren).foreach { _ =>
      val (newRestInput, restMetadata) = extractIndexedMetadata(restInput)
      metadata = metadata ::: restMetadata :: Nil
      restInput = newRestInput
    }
    val newMetadata = if (nChildren == 0) {
      restInput.take(nMetadata)
    } else {
      restInput.take(nMetadata).flatMap(indexOfMetadata =>
        if (indexOfMetadata > metadata.size) List()
        else metadata(indexOfMetadata - 1)
      )
    }
    (restInput.drop(nMetadata), newMetadata)
  }

  def part2(input: Input): Int = extractIndexedMetadata(input)._2.sum

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day8/input.txt").getLines.next
      .split(' ')
      .map(_.toInt)
      .toList
    println(part1(input))
    println(part2(input))
  }
}
