
val lines = scala.io.Source.fromFile("day1-input.txt").mkString
//val lines = scala.io.Source.fromFile("day1-example.txt").mkString

val pairs = lines
  .split("\n")
  .map(str => str.split("  "))

val left = pairs.map(x => x(0).trim().toInt).sorted()
val right = pairs.map(x => x(1).trim().toInt).sorted()

println(left.mkString(","))
println(right.mkString(","))

val zipped = left zip right

println(zipped.mkString(","))

val diff = zipped.map(pair => (pair(0) - pair(1)).abs)

println(diff.mkString(","))

val sum = diff.fold(0)((acc, cur) => acc + cur)

println(sum)
// f.foreach(x => println(x(0) + "," + x(1)))

