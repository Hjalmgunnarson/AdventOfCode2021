package adventOfCode

import scala.collection.mutable

object Day15 extends App {
  val map = mutable.HashMap(1 -> (0, 0), 20 -> (1, 1), 8 -> (2, 2), 12 -> (3, 3), 0 -> (4, 4), 14 -> (5, 5))
  var i = map.size
  var lastEntry = 14
  while (i < 30000000) {
    if (i % 1000 == 0) println(i)
    map.get(lastEntry) match {
      case Some((indexNewer: Int, indexOlder: Int)) =>
        lastEntry = indexNewer - indexOlder
        map.update(lastEntry, (i, map.getOrElse(lastEntry, (i, i))._1))
        i += 1
    }
  }
  println(i + " " + lastEntry)

}
