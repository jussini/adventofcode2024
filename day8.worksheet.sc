import scala.util.matching.Regex
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.HashSet
val input = scala.io.Source.fromFile("day8-input.txt").mkString
val example = scala.io.Source.fromFile("day8-example.txt").mkString
val example2 = scala.io.Source.fromFile("day8-example2.txt").mkString
val example3 = scala.io.Source.fromFile("day8-example3.txt").mkString
val example4 = scala.io.Source.fromFile("day8-example4.txt").mkString
val example5 = scala.io.Source.fromFile("day8-example5.txt").mkString
val trivial1: String = List(
    "..A....",
    "....A..",
    ".....A."
).mkString("\n")

val trivial2: String = List(
    "....",
    ".aa.",
    ".aa.",
    "....",
).mkString("\n")

val trivial3 = "....A..A.....f..f..."

val target = input

class Coordinate(_x: Int, _y: Int):
    def x: Int = _x
    def y: Int = _y
    override def toString(): String = s"($_x, $_y)"
    override def equals(that: Any): Boolean = that match
        case c: Coordinate => c.x == _x && c.y == _y
        case _ => false
    override def hashCode(): Int = s"($_x, $_y)".hashCode()

class Antenna(_c: Coordinate, _s: Char):
    def coordinate = _c
    def symbol = _s
    override def toString(): String = s"$_s$_c"
    override def equals(that: Any): Boolean = that match
        case a: Antenna => a.coordinate == _c && a.symbol == _s
        case _ => false

def printMap(map: List[String]): Unit = map.foreach(row => println(row.mkString))

val rows = target.split("\n").toList
val debug = rows.length <= 12

if (debug) printMap(rows)

val symbolRegex: Regex = "([a-z]|[A-Z]|[0-9])".r
val antennasByFreq = rows.zipWithIndex.flatMap(
    (row, rowIndex) => row.zipWithIndex.map(
        (a, colIndex) => Antenna(Coordinate(colIndex, rowIndex), a)
    )
)
.filter(_.symbol.toString match
    case symbolRegex(s) => true
    case _ => false 
)
.groupBy(_.symbol)

val antennaPairs = antennasByFreq.values.flatMap(antennas => 
    antennas.flatMap(antenna =>
        val rest = antennas.filter(x => x != antenna)
        rest.map(r => (antenna, r))
    )
).toList

val pointOnMap: (Coordinate) => Boolean = (c) => c.x >= 0 && c.y >= 0 && c.x < rows.apply(0).length && c.y < rows.length

val antinodes = antennaPairs.map((a, b) => Coordinate(
    b.coordinate.x + (b.coordinate.x - a.coordinate.x),
    b.coordinate.y + (b.coordinate.y - a.coordinate.y)
)).to(HashSet).filter(pointOnMap)

def printMapWithAntinodes(map: List[String], antinodesOn: HashSet[Coordinate]): Unit = map.zipWithIndex.foreach((row, rowIndex) => {
    val d = row.zipWithIndex.map((c, colIndex) => {
        if (antinodesOn.contains(Coordinate(colIndex, rowIndex))) '#' else c
    }).mkString
    println(d)
})

if (debug) printMapWithAntinodes(rows, antinodes)

println("Part 1, number of unique antinode locations on map: " + antinodes.size)

val antinodesp2 = antennaPairs.flatMap((a, b) => {
    val unit = Coordinate(b.coordinate.x - a.coordinate.x, b.coordinate.y - a.coordinate.y)
    // this might be horribly wrong, but i assume the antennas are positioned so that the vector between any pair cannot
    // be factored down.
    LazyList.from(0).map(i => Coordinate(a.coordinate.x + i * unit.x, a.coordinate.y + i * unit.y)).takeWhile(pointOnMap)
}).to(HashSet)

if (debug) printMapWithAntinodes(rows, antinodesp2)

println("Part 2, number of unique antinodes " + antinodesp2.size)
