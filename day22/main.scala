import scala.io.Source
import scala.util.matching.Regex

final case class Coord(x: Int, y: Int)

final case class Node(size: Int, used: Int, avail: Int)

type Entry = (Coord, Node)

def parseEntry(s: String): Entry =
  "\\d+".r.findAllMatchIn(s).map(m => m.toString.toInt).toList match
    case x::y::size::used::avail::_ => (Coord(y, x), Node(size, used, avail))
    case _ => throw new Exception("Bad input")

def valids(ns: List[Entry]): Int =
  (for (c0, n0) <- ns
       (c1, n1) <- ns
       if c0 != c1 && n0.used != 0 && n0.used <= n1.avail
   yield ()).length

def solveFile(fp: String): Unit =
  println(s"Solving for file : $fp")
  val entries = Source.fromFile(fp).getLines.drop(2).map(parseEntry)
  println(s"Part 1 : ${valids(entries.toList)}")

@main def main(args: String *) = args.foreach(solveFile)
