import java.io.File
import java.security.MessageDigest
import kotlin.text.Charsets.UTF_8

const val OPEN_CHARS = "bcdef"

data class Dir(val i: Int, val dir: Char, val dx: Int, val dy: Int)

val directions = arrayOf(
  Dir(0, 'U', 0, -1),
  Dir(1, 'D', 0, 1),
  Dir(2, 'L', -1, 0),
  Dir(3, 'R', 1, 0),
)

data class State(val coord: Coord, val path: String)

fun pathsTo(goal: Coord, next: (s: State) -> Array<State>, initialState: State): ArrayList<State> {
  val q = ArrayQueue<State>()
  q.insert(initialState)
  val path = ArrayList<State>()
  while (!q.empty()) {
    val t = q.pop()
    if (t.coord != goal) next(t).forEach { q.insert(it) } else path.add(t)
  }
  return path
}

data class Coord(val x: Int, val y: Int)

fun Coord.valid() =
  0 <= this.x && this.x < 4 && 0 <= this.y && this.y < 4

val START = Coord(0, 0)
val GOAL = Coord(3, 3)

fun nextStates(input: String, s: State): Array<State> =
  directions.fold(arrayOf<State>()) { acc, d ->
    val p = Coord(s.coord.x + d.dx, s.coord.y + d.dy)
    if (OPEN_CHARS.indexOf(hash(input + s.path)[d.i]) != -1 && p.valid())
        arrayOf(*acc, *arrayOf(State(p, s.path + d.dir)))
    else acc }


val initialState = State(START, "")

fun solveFile(fp: String) {
  val rawInput = File(fp).readText().trim()
  val goals = pathsTo(GOAL, { s -> nextStates(rawInput, s) }, initialState)
  println("Part 1 : ${goals[0].path}")
  println("Part 2 : ${goals[goals.size - 1].path.length}")
}

fun main(args: Array<String>) =
  args.forEach(::solveFile)

fun md5(s: String): ByteArray =
  MessageDigest.getInstance("MD5").digest(s.toByteArray(UTF_8))

fun ByteArray.toHex() =
  joinToString(separator = "") { b -> "%02x".format(b) }

fun hash(s: String) = md5(s).toHex()

interface Queue<T> {
  fun insert(item: T): Queue<T>
  fun pop(): T
  fun empty(): Boolean
}

class ArrayQueue<T> : Queue<T> {
  var rep: ArrayList<T>
  init { this.rep = ArrayList<T>() }
  override fun insert(item: T): Queue<T> {
    rep.add(item)
    return this
  }
  override fun pop() =
    this.rep.removeFirst()
  override fun empty() =
    this.rep.size == 0
  override fun toString(): String =
    this.rep.toString()
}
