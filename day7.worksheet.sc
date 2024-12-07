import scala.util.matching.Regex
val input = scala.io.Source.fromFile("day7-input.txt").mkString

val example = List(
"190: 10 19",
"3267: 81 40 27",
"83: 17 5",
"156: 15 6",
"7290: 6 8 6 15",
"161011: 16 10 13",
"192: 17 8 14",
"21037: 9 7 18 13",
"292: 11 6 16 20",
).mkString("\n")

val target = input

def parserRegex: Regex = "(\\d+): (.*)".r

val lines = target.split("\n").map((line) => 
    line match
        case parserRegex(testValue, numbers) => (
            BigInt.apply(testValue),
            numbers.split(" ").map(BigInt.apply(_)).toList
        )
        case waitwat => throw new RuntimeException("Could not parse line " + waitwat)
)

type OperatorList = List[(BigInt, BigInt) => BigInt]
val operators: OperatorList = List(
    (a: BigInt, b: BigInt) => a + b,
    (a: BigInt, b: BigInt) => a * b
)


def applyOperators(ops: OperatorList, numbers: List[BigInt]): List[BigInt] = {
    numbers match
        case Nil => List()
        case a :: Nil => List(a)
        case a :: b :: rest => {
            val partials = ops.map(op => op(a, b))
            partials.flatMap(partial => applyOperators(ops, partial :: rest))
        }    
}

val f = applyOperators(operators, lines.last._2)

val possibleTests = lines.map(line => {
    val lineResults = applyOperators(operators, line._2)
    lineResults.indexOf(line._1) match
        case -1 => 0: BigInt
        case _ => line._1 
})

println("Day 7 part 1: " + possibleTests.reduce(_+_))


val operatorsPart2: OperatorList = List(
    (a: BigInt, b: BigInt) => a + b,
    (a: BigInt, b: BigInt) => a * b,
    (a: BigInt, b: BigInt) => BigInt.apply(a.toString() + b.toString())
)

val possibleTestsPart2 = lines.map(line => {
    val lineResults = applyOperators(operatorsPart2, line._2)
    lineResults.indexOf(line._1) match
        case -1 => 0: BigInt
        case _ => line._1 
})

println("Day 7 part 2: " + possibleTestsPart2.reduce(_+_))