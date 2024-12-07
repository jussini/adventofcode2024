import scala.util.matching.Regex
val input = scala.io.Source.fromFile("day5-input.txt").mkString
val example = scala.io.Source.fromFile("day5-example.txt").mkString

val lines = input.split("\n")

val ruleRegex: Regex = "(\\d+)\\|(\\d+)".r


val rulesAndUpdates = lines.map((line) => 
    line match
        case ruleRegex(left, right) => (left.toInt, right.toInt)    
        case "" => ""
        case update => update.split(",")
)

val rules = rulesAndUpdates.filter(x => 
    x match
        case x: (Int, Int) => true
        case _ => false
).map(x => 
    (x: @unchecked) match
        case x: (Int, Int) => x
)

val updates = rulesAndUpdates.filter(x => 
    x match
        case x: Array[String] => true
        case _ => false
)
.map(x => 
    (x: @unchecked) match
        case x: Array[String] => x.map((s) => s.toInt)
)

val correctOrdered = updates.filter((update) => {
    val sorted = update.sortWith((left, right) => rules.exists((ruleLeft, ruleRight) => left == ruleLeft && right == ruleRight))
    sorted sameElements update
})


val middlePages = correctOrdered.map((order) => order.apply(order.length / 2))

val sum = middlePages.reduce(_+_)

println("Day 5 part 1: " + sum)


val incorrectlyOrdered = updates.filter((update) => {
    val sorted = update.sortWith((left, right) => rules.exists((ruleLeft, ruleRight) => left == ruleLeft && right == ruleRight))
    if (sorted sameElements update) false else true
})

val fixed = incorrectlyOrdered.map(_.sortWith((left, right) => rules.exists((ruleLeft, ruleRight) => left == ruleLeft && right == ruleRight)))

val fixedMiddlePages = fixed.map((x) => x.apply(x.length / 2))

val fixewdSum = fixedMiddlePages.reduce(_+_)

println("Day 5 part 2: " + fixewdSum)