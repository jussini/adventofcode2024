
val input = scala.io.Source.fromFile("day11-input.txt").mkString.split(" ").map(BigInt(_)).toList

def transform(number: BigInt): List[BigInt] = {
    val numDigits = scala.math.log10(number.toDouble).floor.toInt + 1

    if (number == 0) {
        return List(1)
    }
    if (numDigits % 2 == 0) {
        val N = scala.math.pow(10, (numDigits / 2)).toInt
        return List(number / N, number % N)
    }
    List(number * 2024)
}

val test: List[BigInt] = List(125, 17)
val target = input

def blink(numbers: List[BigInt]): List[BigInt] = {
    numbers.flatMap(transform)
}

val blinks = 25
val result = List.range(0, blinks).foldLeft(target)((acc, _) => {
    blink(acc)
})

println(s"After $blinks blinks, number of stones: " + result.size)

