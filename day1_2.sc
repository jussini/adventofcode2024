
// val lines = scala.io.Source.fromFile("day1-input.txt").mkString
val lines = scala.io.Source.fromFile("day1-example.txt").mkString

val pairs = lines
  .split("\n")
  .map(str => str.split("  "))

val left = pairs.map(x => Integer(x(0).trim()))
val right = pairs.map(x => Integer(x(1).trim()))

println(left.mkString(","))
println(right.mkString(","))

/*
This time, you'll need to figure out exactly how often each number from the
left list appears in the right list. Calculate a total similarity score 
by adding up each number in the left list after multiplying it by the number of times 
that number appears in the right list.
...

So, for these example lists, the similarity score at the end of 
this process is 31 (9 + 4 + 0 + 0 + 9 + 9).
*/

val leftProducts = left.map(leftnum => {
    val count = right.filter(rightnum => leftnum == rightnum).length
    leftnum * count
})

println(leftProducts.mkString(","))

println()
println(leftProducts.reduce(_ + _))