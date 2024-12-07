import scala.util.matching.Regex
import scala.annotation.tailrec

val input = scala.io.Source.fromFile("day3-input.txt").mkString
val example1 = scala.io.Source.fromFile("day3-example.txt").mkString
// mul(X,Y), where X and Y are each 1-3 digit numbers.

val mulRegx: Regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r

def exctractMul(str: String): (Int, Int) = 
    str match
        case mulRegx(left, right) => (left.toInt, right.toInt)

val matches = mulRegx.findAllIn(input).toList.map(exctractMul)
        
val sum = matches.map((left, right) => left * right).reduce(_ + _)
println(s"Part 1 sum: $sum")
