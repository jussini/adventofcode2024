val input = scala.io.Source.fromFile("day11-input.txt").mkString.split(" ").map(BigInt(_)).toList

val cache = scala.collection.mutable.Map[(BigInt, Int), BigInt]()

def transform(number: BigInt): (BigInt, BigInt) | BigInt = {
    val numDigits = scala.math.log10(number.toDouble).floor.toInt + 1

    if (number == 0) {
        return 1
    }
    if (numDigits % 2 == 0) {
        val N = scala.math.pow(10, (numDigits / 2)).toInt
        return (number / N, number % N)
    }
    number * 2024
}

val test: List[BigInt] = List(125, 17)
val target = input

def blink(number: BigInt, level: Int, levels: Int): BigInt = {
    if (level == levels) {
        return 1
    }

    val cacheKey = (number, levels - level)
    if (cache.contains(cacheKey)) {
        val cacheVal = cache(cacheKey)
        return cacheVal
    }

    val numStones = transform(number) match
        case n: BigInt => blink(n, level + 1, levels)
        case (a, b) => blink(a, level + 1, levels) + blink(b, level + 1, levels)

    cache(cacheKey) = numStones
    numStones
}

val sum25 = target.map(num => {
    val numval = blink(num, 0, 25)
    numval
}).sum

println("After 25 blinks: " + sum25)

val sum75 = target.map(num => {
    val numval = blink(num, 0, 75)
    numval
}).sum

println("After 75 blinks: " + sum75)