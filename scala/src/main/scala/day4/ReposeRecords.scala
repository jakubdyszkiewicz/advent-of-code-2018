package day4

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.io.Source

object ReposeRecords {

  implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

  case class Record(date: LocalDateTime, msg: String)

  private val recordRegex = raw"(\[)(.+)(\])(.*)".r
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def parseRecord(line: String): Record = line match {
    case recordRegex(_, date, _, msg) => Record(LocalDateTime.parse(date, formatter), msg)
  }

  case class Nap(fromMinute: Int, toMinute: Int, guard: Int) {
    def minutesSlept: Int = toMinute - fromMinute + 1

    def inRange(minute: Int): Boolean = minute >= fromMinute && minute <= toMinute
  }

  private val guardShiftRegex = raw"(.*)(Guard #)(\d+)(.*)".r

  def napsFromRecords(
    records: Seq[Record],
    naps: List[Nap] = List(),
    prevGuard: Int = -1,
    prevRecordDate: LocalDateTime = LocalDateTime.now()
  ): List[Nap] = {
    if (records.isEmpty) {
      return naps
    }
    records.head.msg match {
      case guardShiftRegex(_, _, guard, _) =>
        napsFromRecords(records.tail, naps, guard.toInt)
      case msg if msg.contains("falls asleep") =>
        napsFromRecords(records.tail, naps, prevGuard, records.head.date)
      case msg if msg.contains("wakes up") =>
        napsFromRecords(records.tail,
          Nap(
            fromMinute = prevRecordDate.getMinute,
            toMinute = records.head.date.minusMinutes(1).getMinute,
            guard = prevGuard) :: naps,
          prevGuard)
    }
  }

  type NapsByGuard = Map[Int, Seq[Nap]]

  def mostSleepyGuard(naps: NapsByGuard): Int =
    naps
      .mapValues(naps => naps.map(_.minutesSlept).sum)
      .maxBy { case (_, minutesSlept) => minutesSlept }
      ._1

  def favouriteMinuteToSleep(naps: Seq[Nap]): (Int, Int) =
    (0 to 59)
      .map(minute => minute -> naps.count(_.inRange(minute)))
      .maxBy { case (_, sleeps) => sleeps }

  def mostFrequentAsleepOnSameMinute(naps: NapsByGuard): (Int, Int) = {
    val (guard, (minute, _)) = naps
      .mapValues(favouriteMinuteToSleep)
      .maxBy { case (guard, (minute, sleeps)) => sleeps }
    (guard, minute)
  }

  def main(args: Array[String]): Unit = {
    val records = Source.fromResource("day4/input.txt").getLines
      .map(parseRecord)
      .toSeq
      .sortBy(_.date)

    val naps = napsFromRecords(records)
    val napsByGuard = naps.groupBy(_.guard)

    val guard = mostSleepyGuard(napsByGuard)
    val (favouriteMinute, _) = favouriteMinuteToSleep(napsByGuard(guard))

    println(s"Most sleepy guard is $guard with favourite minute $favouriteMinute." +
      s"The Answer is ${guard * favouriteMinute}")

    val (guard2, minute) = mostFrequentAsleepOnSameMinute(napsByGuard)
    println(s"Most frequent asleep guard on the same minute is guard $guard2: with minute $minute." +
      s"The answer is ${guard2 * minute}")

  }
}
