import scala.io.Source
import scala.util.matching.Regex
import scala.collection.Map
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

@main def main(args: String *) = args.foreach(solveFile)

final case class Coord(x: Int, y: Int)

given Ordering[Coord] = (a: Coord, b: Coord) =>
  if (a.x > b.x) then 1 else if (a.x < b.x) then -1 else 0

def cardinal(c: Coord): List[Coord] =
  (Coord(c.x + 1, c.y) :: Coord(c.x - 1, c.y) ::
   Coord(c.x, c.y + 1) :: Coord(c.x, c.y - 1) :: Nil)

final case class Node(size: Int, used: Int, avail: Int)

type Entry = (Coord, Node)

def parseEntry(s: String): Entry =
  "\\d+".r.findAllMatchIn(s).map(m => m.toString.toInt).toList match
    case x::y::size::used::avail::_ => (Coord(x, y), Node(size, used, avail))
    case _ => throw new Exception("Bad input")

def valids(ns: List[Entry]): Int =
  (for (c0, n0) <- ns
       (c1, n1) <- ns
       if c0 != c1 && n0.used != 0 && n0.used <= n1.avail
   yield ()).length

def solveFile(fp: String): Unit =
  println(s"Solving for file : $fp")
  val entries = Source.fromFile(fp).getLines.drop(2).map(parseEntry).toList
  println(s"Part 1 : ${valids(entries)}")
  val start = (for (c, n) <- entries if c.y == 0 yield c).max
  val empty = entries.find(_(1).used == 0) match
    case Some(e) => e(0)
    case None => throw new Exception("No empty node found.")
  val path = astar(SearchState.init(start, empty), SearchState.distance, SearchState.nextMoves(grid(entries)))
  println(s"Part 2 : ${path.length - 1}")

final case class SearchState(datapos: Coord, emptypos: Coord)

type Grid = Map[Coord, Entry]

def grid(es: List[Entry]): Grid =
  es.foldLeft(Map.empty){ (acc, x) => acc + (x(0) -> x) }

object SearchState {
  def init(datapos: Coord, emptypos: Coord): SearchState = SearchState(datapos, emptypos)

  def nextMoves(grid: Grid)(state: SearchState): List[SearchState] =
    for pos <- cardinal(state.emptypos)
        if grid contains pos
        if (grid get pos).get(1).used <= (grid get state.emptypos).get(1).size
    yield SearchState((if pos == state.datapos then state.emptypos else state.datapos), pos)

  def distance(s: SearchState): Int =
    val target = Coord(0, 0)
    Math.abs(s.datapos.x - target.x) + Math.abs(s.datapos.y - target.y)
}

type PQI = (Int, SearchState)
type PQ = PriorityQueue[PQI]

given Ordering[(Int, SearchState)] = (a: PQI, b: PQI) =>
  if a(0) < b(0) then 1 else -1

def astar(init: SearchState, h: SearchState => Int, moves: SearchState => List[SearchState]): List[SearchState] =
  var pq: PQ = PriorityQueue((h(init), init))
  var prev: mutable.Map[SearchState, Option[SearchState]] = mutable.Map.empty += (init -> None)
  var pathCost = mutable.Map.empty += (init -> 0)
  while (pq.size != 0) {
    val (_, x) = pq.dequeue
    if (h(x) == 0) {
      return path(prev, Some(x))
    }
    moves(x).foreach { x2 =>
      val newCost = (pathCost get x).get + 1
      if (!(pathCost contains x2) ||
          ((pathCost contains x2) && newCost < (pathCost get x2).get)) {
        pq.+=((newCost + h(x2), x2))
        pathCost += (x2 -> newCost)
        prev += (x2 -> Some(x))
      }
    }
  }
  return Nil

def path(prev: Map[SearchState, Option[SearchState]], s: Option[SearchState]): List[SearchState] =
  s match
    case None => Nil
    case Some(s) => path(prev, (prev get s).flatten) ++ (s :: Nil)
