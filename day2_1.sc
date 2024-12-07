import scala.util.boundary, boundary.break

val lines = scala.io.Source.fromFile("day2-input.txt").mkString.split("\n")
// val lines = scala.io.Source.fromFile("day2-example.txt").mkString.split("\n")

/* 
The unusual data (your puzzle input) consists of many reports, one report per line. 
Each report is a list of numbers called levels that are separated by spaces. For example:

7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
This example data contains six reports each containing five levels.

So, a report only counts as safe if both of the following are true:
- The levels are either all increasing or all decreasing.
- Any two adjacent levels differ by at least one and at most three.
 */

def increasing(x: Array[Int]): Boolean = x.zipWithIndex.foldRight(true)((cur, acc) => {
    val value = cur(0)
    val index = cur(1)
    boundary:
        if (!acc)
            break(false)
        if (index == 0)
            break(true)
        val previousValue = x(index - 1)
        value > previousValue
})

def decreasing(x: Array[Int]): Boolean = x.zipWithIndex.foldRight(true)((cur, acc) => {
    val value = cur(0)
    val index = cur(1)
    boundary:
        if (!acc)
            break(false)
        if (index == 0)
            break(true)
        val previousValue = x(index - 1)
        value < previousValue
})

def safeDiffering(x: Array[Int]): Boolean = x.zipWithIndex.foldRight(true)((cur, acc) => {
    val value = cur(0)
    val index = cur(1)
    boundary:
        if (!acc)
            break(false)
        if (index == 0)
            break(true)
        val previousValue = x(index - 1)
        (value - previousValue).abs <= 3
})

val reports = lines.map(line => line.split(" ").map(_.toInt))

val safeRepots = reports.filter(x => (increasing(x) || decreasing(x)) && safeDiffering(x))
safeRepots.foreach(x => println(x.mkString(",")))

println()
println("Number of safe reports Part 1: " + safeRepots.length)

val reportVariations: (report: Array[Int]) => IndexedSeq[Array[Int]] = (report) => {
    val thing = (0 until report.length).map(i => report.zipWithIndex.filter(_._2 != i).map(_._1))
    thing
}

// println("Report variations for " + reports(0).mkString(","))
// reportVariations(reports(0)).foreach(x => println(x.mkString(",")))

val dampedSafeRepots = reports.filter(report =>
    reportVariations(report).exists(x => (increasing(x) || decreasing(x)) && safeDiffering(x))    
)

println("Number of dampened safe reports Part 2: " + dampedSafeRepots.length)