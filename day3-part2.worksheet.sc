import scala.util.matching.Regex
import scala.annotation.tailrec

val input = scala.io.Source.fromFile("day3-input.txt").mkString
val example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
// mul(X,Y), where X and Y are each 1-3 digit numbers.

val mulRegx: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r

def exctractMul(str: String): Either[(Int, Int), Boolean] = 
    println("extract from " + str)
    str match
        case mulRegx(left, right) => Left((left.toInt, right.toInt))
        case "do()" => Right(true)
        case "don't()" => Right(false)

val mulRegx2: Regex = "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r
val operations = mulRegx2.findAllIn(input).toList.map(exctractMul)
println(operations)

def filterByMode(mode: Boolean, ops: List[Either[(Int, Int), Boolean]]): List[(Int, Int)] = {
    ops match
        case Right(false) :: rest => filterByMode(false, rest)
        case Right(true) :: rest => filterByMode(true, rest)
        case Left(a, b) :: rest => if(mode) (a, b) :: filterByMode(mode, rest) else filterByMode(mode, rest)
        case Nil => List()    
}

val validOps = filterByMode(true, operations)
val sum = validOps.map((left, right) => left * right).reduce(_ + _)

println("Part 2 sum: " + sum)