import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer

val input = scala.io.Source.fromFile("day6-input.txt").mkString
val example = scala.io.Source.fromFile("day6-example.txt").mkString

val trivial = List(
    "##...",
    "....#",
    ".....",
    "^...#",
    "#....",
    "....."
    ).mkString("\n")

val trivial2 = List(
    "##...",
    "....#",
    "...#.",
    "^....",
    "#....",
    "....."
    ).mkString("\n")

val target = input

object CellType extends Enumeration {
  type CellType = Value
  val Block, PathWay, Guard = Value
}
import CellType._

object PathVector extends Enumeration {
  type PathVector = Value
  val Left, Right, Up, Down = Value
}
import PathVector._

abstract class MapCell:
    val cellType: CellType

class Block extends MapCell:
    val cellType = Block
    override def toString(): String = "#"

class PathWay extends MapCell:
    val cellType = PathWay
    override def toString(): String = "."

class Guard(_vector: PathVector) extends MapCell:
    val cellType = Guard
    def vector: PathVector = _vector
    override def toString(): String = _vector match
        case Up => "^"
        case Right => ">"
        case Down => "v"
        case Left => "<"
        case _ => "x" 
    

type Map = Array[Array[MapCell]]

class Coordinate(_x: Int, _y: Int):
    def x: Int = _x
    def y: Int = _y
    override def toString(): String = s"($_x, $_y)"
    override def equals(that: Any): Boolean = that match
        case c: Coordinate => c.x == _x && c.y == _y
        case _ => false

class TraceItem(_coordinate: Coordinate, _vector: PathVector):
    def coordinate = _coordinate
    def vector = _vector
    override def toString(): String = s"$_coordinate to $_vector"
    override def equals(that: Any): Boolean = that match
        case ti: TraceItem => ti.coordinate.x == _coordinate.x && ti.coordinate.y == _coordinate.y && ti.vector == _vector
        case _ => false
    

val map: Map = target.split("\n").map(_.split("").map( 
    _ match
        case "." => PathWay()
        case "#" => Block()
        case "^" => Guard(Up)
        case ">" => Guard(Right)
        case "v" => Guard(Down)
        case "<" => Guard(Left))
)

val isGuardCell: MapCell => Boolean =
    _ match
        case x: Guard => true
        case d => false
    
val initialRowIndex = map.indexWhere(_.exists(isGuardCell))
val initialColIndex = map.apply(initialRowIndex).indexWhere(isGuardCell)
var guardInitialLocation = Coordinate(initialColIndex, initialRowIndex)

// type Trace = List[TraceItem]
type Trace = ListBuffer[TraceItem]

val newPosition: (Guard, Coordinate) => Coordinate = (guard, from) => {
    guard.vector match
        case Up => Coordinate(from.x, from.y - 1)
        case Down => Coordinate(from.x, from.y + 1)
        case Right => Coordinate(from.x + 1, from.y)
        case Left => Coordinate(from.x - 1, from.y)
}

val rotate: PathVector => PathVector =
    _ match
        case Up => Right
        case Right => Down
        case Down => Left
        case Left => Up    

val pointIsOnMap: (Map, Coordinate) => Boolean = (onMap, coordinate) => 
    coordinate.y >= 0 && coordinate.y < onMap.length && coordinate.x >= 0 && coordinate.x < onMap.apply(0).length

val getCell: (Map, Coordinate) => MapCell = (onMap, coord) => {
    val c = onMap.apply(coord.y).apply(coord.x)
    c
}

val initialGuard = getCell(map, guardInitialLocation) match
    case g: Guard => g
    case _ => throw new RuntimeException(s"There was supposed to be guard at $guardInitialLocation")

def move(onMap: Map, guard: Guard, from: Coordinate, trace: Trace, loopBlocks: List[Coordinate]): Trace = {
    // did we fall off?, cool
    if (!pointIsOnMap(onMap, from)) {
        return trace
    }

    val cell = getCell(onMap, from)

    // if on block, trace back and get a new vector
    if (cell.cellType == Block) {
        val newVect = rotate(guard.vector)
        val newGuard = Guard(newVect)
        val lastPosition = trace.last
        return move(onMap, newGuard, lastPosition.coordinate, trace, loopBlocks)
    }


    // otherwise, keep on going and 
    val newFrom = newPosition(guard, from)
    trace.append(TraceItem(from, guard.vector))
    move(onMap, guard, newFrom, trace, loopBlocks)
}

def traversesToLoop(onMap: Map, guard: Guard, from: Coordinate, trace: Trace, extraBlockAt: Coordinate): Boolean = {
    // did we fall off?, cool no loop
    if (!pointIsOnMap(onMap, from)) {
        return false
    }

    // println("guard " + guard + " on " + from + " " + getCell(onMap, from))

    val cell = onMap.apply(from.y).apply(from.x)

    // have we been on this spot before, going same direction?
    if (trace.indexWhere(ti => ti.coordinate.x == from.x && ti.coordinate.y == from.y && ti.vector == guard.vector) != -1) {
        // println("found loop " + trace.mkString(" "))
        return true
    }

    // if on block, trace back and get a new vector
    if (from == extraBlockAt || cell.cellType == Block) {
        val newVect = rotate(guard.vector)
        val newGuard = Guard(newVect)
        val lastPosition = trace.last
        return traversesToLoop(onMap, newGuard, lastPosition.coordinate, trace, extraBlockAt)
    }
    
    // otherwise, keep on going and 
    val newFrom = newPosition(guard, from)
    trace.append(TraceItem(from, guard.vector))
    traversesToLoop(onMap, guard, newFrom, trace, extraBlockAt)
}


val path: Trace = ListBuffer()
    
val part1trace = move(map, initialGuard, guardInitialLocation, path, List())

println("part 1 trace " + part1trace)

println("positions " + part1trace.distinctBy(_.coordinate.toString()))

val numDistinct = part1trace.distinctBy(_.coordinate.toString()).length
println("Day 6 part 1: " + numDistinct)



def printMap(map: Map): Unit = map.foreach(row => println(row.mkString))

//printMap(map)
//println()


val coords = part1trace.distinctBy(_.coordinate.toString())
    .map(ti => ti.coordinate)
    .filter(coord => {
        val cell = getCell(map, coord)
        if (coord == guardInitialLocation || coord == newPosition(initialGuard, guardInitialLocation) || (cell.cellType == Block)) false
        else true
    })


def loopCheck(onMap: Map, guard: Guard, from: Coordinate, extraBlockAt: Coordinate, message: String): Future[Boolean] = Future {
    val x = traversesToLoop(onMap, guard, from, ListBuffer(), extraBlockAt)
    // if (x) println("Extra block at " + extraBlockAt + " leads to loop - " + message)
    x
}

val promises = coords.zipWithIndex.map((c, testIdx) => {
    // println("Check " + c + ", " + (testIdx + 1) + "/" + coords.length)
    loopCheck(map, initialGuard, guardInitialLocation, c, (testIdx + 1) + "/" + coords.length)
})

val seq = Future.sequence(promises)

seq.onComplete {
    results => 
        println("Finally got results ") 
        val sum = results.get.map(r => if (r) 1 else 0).reduce(_+_)
        println("Num successful loop blocks " + sum)
        sys.exit(0)
}

println("And now we wait...")
Thread.sleep(1000 * 120)
println("timeout :()")
