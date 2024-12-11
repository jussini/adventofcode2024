import scala.util.Try

val input = scala.io.Source.fromFile("day10-input.txt").mkString
val example = scala.io.Source.fromFile("day10-example.txt").mkString

val example2 = List(
    "0123",
    "1234",
    "8765",
    "9876",
).mkString("\n")

val example3 = List(
"...0...",
"...1...",
"...2...",
"6543456",
"7.....7",
"8.....8",
"9.....9",
).mkString("\n")

val example4 = List(
"10..9..",
"2...8..",
"3...7..",
"4567654",
"...8..3",
"...9..2",
".....01",
).mkString("\n")

val target = input

type Coord = (Int, Int)

// type Height = Int
class Height(_h: Int):
    def h: Int = _h
    override def toString(): String = if (_h < 0) "." else if (_h >= 10) "#" else s"$_h"
    override def equals(that: Any): Boolean = that match
        case c: Height => c.h == _h
        case _ => false
    override def hashCode(): Int = s"HEIGHT$_h".hashCode()


type MapItem = (Height, Coord)
type Row = Array[MapItem]
type Map = Array[Row]

val Up = (0, -1)
val Down = (0, 1)
val Left = (-1, 0)
val Right = (1, 0)

val inputMap: Map = target.split("\n")
.zipWithIndex
.map((row, rowIndex) => row.split("").zipWithIndex.map((c, colIndex) => 
    val height = Try(c.toInt).toOption match
        case None => -1
        case Some(i) => i
    
    (Height(height), (colIndex, rowIndex)) ))

val inputItems: List[MapItem] = inputMap.map(row => row.toList).toList.flatten

val trailHeads = inputItems.filter(mi => mi._1.h == 0)

def findAround(at: MapItem): List[MapItem] = List(Up, Down, Left, Right).map((dx, dy) => {
    val newX = at._2._1 + dx
    val newY = at._2._2 + dy
    (newX, newY) 
}).filter((x, y) => x >= 0 && y >= 0 && x < inputMap.apply(0).size && y < inputMap.size)
.map((x, y) => inputMap.apply(y).apply(x))
.filter(mapItem => mapItem._1.h == at._1.h + 1)


def findPeaks(from: MapItem, onMap: Map, trail: List[MapItem]): List[(MapItem, List[MapItem])] = {
    if (from._1.h == 9)
        return List((from, trail))
    val around = findAround(from)
    if (around.isEmpty) List() else around.flatMap(x => findPeaks(x, onMap, (from :: trail)))
}

def printMap(map: Map): Unit = map.foreach(row => println(row.map(_._1).mkString) )

val scores = trailHeads.map(head => 
    val trailPeaks = findPeaks(head, inputMap, List())
    val distinct = trailPeaks.distinctBy(p => p._1)
    distinct.size
)

println("Part 1 sum of scores: " + scores.reduce(_+_))

val ratings = trailHeads.map(head => 
    val trailPeaks = findPeaks(head, inputMap, List())
    trailPeaks.size
)

println("Part 2 sum of ratings: " + ratings.reduce(_+_))