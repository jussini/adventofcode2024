import scala.util.matching.Regex
val input = scala.io.Source.fromFile("day4-input.txt").mkString
val example = scala.io.Source.fromFile("day4-example.txt").mkString
/*
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
*/
// val example = List(
//     "abcd",
//     "efgh",
//     "ijkl",
//     "mnop"
// ).mkString("\n")


val rows = input.split("\n").toList
val cols = rows.transpose.map((charlist) => charlist.mkString)

def collectDiagDown(puzzle: List[String], rowIndex: Int, colIndex: Int): String =
    if (rowIndex > puzzle.length - 1 || colIndex > puzzle.apply(rowIndex).length - 1) "" 
    else puzzle.apply(rowIndex).substring(colIndex, colIndex + 1) + collectDiagDown(puzzle, rowIndex + 1, colIndex + 1)

def collectDiagUp(puzzle: List[String], rowIndex: Int, colIndex: Int): String =
    if (rowIndex < 0 || colIndex > puzzle.apply(rowIndex).length - 1) "" 
    else puzzle.apply(rowIndex).substring(colIndex, colIndex + 1) + collectDiagUp(puzzle, rowIndex - 1, colIndex + 1)

val diag1 = rows.zipWithIndex.map((_, rowIndex) => collectDiagDown(rows, rowIndex, 0))
val diag2 = rows.apply(0).zipWithIndex.map((_, colIndex) => collectDiagDown(rows, 0, colIndex + 1)).toList
val diag3 = cols.zipWithIndex.map((_, rowIndex) => collectDiagUp(rows, rowIndex, 0))
val diag4 = rows.apply(0).zipWithIndex.map((_, colIndex) => collectDiagUp(rows, rows.length - 1, colIndex + 1)).toList

val regex: Regex = "XMAS".r
val xeger: Regex = "SAMX".r
def findXMAS(list: List[String]): Int = {
    val sdf = list.map((s) => {
        val matches = regex.findAllIn(s)
        val rev_matches = xeger.findAllIn(s)
        val l = matches.length + rev_matches.length
        println("Found " + l + " in " + s)
        l
    })
    println(sdf.mkString(",") + " in " + list)
    sdf.reduce(_+_)
}

val a = List(rows, cols, diag1, diag2, diag3, diag4).map(list => findXMAS(list))
val b = a.reduce(_+_)

println("Day 4 part 1: " + b)

def pick(x: Int, y: Int): String = rows.apply(y).substring(x, x + 1)

def hasXMas(x: Int, y: Int): Boolean = 
    if (x == 0 || y == 0 || x >= rows.head.length - 1 || y >= rows.length - 1) return false
    val diag1 = List(pick(x - 1, y - 1), pick(x, y), pick(x + 1, y + 1)).mkString
    val diag2 = List(pick(x - 1, y + 1), pick(x, y), pick(x + 1, y - 1)).mkString
    (diag1 == "MAS" || diag1 == "SAM") && (diag2 == "MAS" || diag2 == "SAM")

val crossMasCount = rows.zipWithIndex.map((row, y) => 
    row.zipWithIndex.map((_,x) => {
        val has = hasXMas(x, y)
        //if (has) {
        //    println(s"found at $x,$y")
        //}
        if (has) 1 else 0
    }).reduce(_+_)    
    ).reduce(_+_)

println("Day 4 part 2: " + crossMasCount)
