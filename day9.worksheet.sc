import scala.annotation.tailrec

val input = scala.io.Source.fromFile("day9-input.txt").mkString
val example = "2333133121414131402"
val example2 = "12345"

val target = input

abstract class Block()

class File(_id: Int) extends Block:
    def id: Int = _id
    override def toString(): String = if (_id < 10) _id.toString() else "#"

class Free() extends Block:
    override def toString(): String = "."

type DiskMap = List[String]

val diskMap: DiskMap = target.split("").toList

val debug = diskMap.size <= 20

final def parseDiskMap(fileId: Int, map: DiskMap, mode: Int): List[Block] = map match
    case Nil => List()
    case block :: rest => {
        val head = mode % 2 match
            case 0 => List.fill(block.toInt)(File(fileId))
            case 1 => List.fill(block.toInt)(Free())
        
        head ::: parseDiskMap(fileId + (mode % 2), rest, mode + 1)
    }

val diskBlocks = parseDiskMap(0, diskMap, 0).toArray

val freeBlocks = diskBlocks.zipWithIndex.filter((block, _) => block match
    case _: Free => true
    case _ => false
)
val freeBlocksIt = freeBlocks.iterator

val fileBlocks: (Array[Block]) => Array[(Block, Int)] = blocks => blocks.zipWithIndex.filter((block, _) => block match
    case _: File => true
    case _ => false
)

if(debug) println(diskBlocks.mkString)

fileBlocks(diskBlocks).reverse.foreach((fileBlock, fileIndex) => {
    if (freeBlocksIt.hasNext) {
        val (freeBlock, freeIndex) = freeBlocksIt.next
        if (fileIndex > freeIndex) {
            diskBlocks(freeIndex) = fileBlock
            diskBlocks(fileIndex) = freeBlock
        }

    }
    if(debug) println(diskBlocks.mkString)
})

val checksum = (blocks: Array[Block]) => blocks.zipWithIndex.map((block, i) => block match
    case f: File => BigInt(i * f.id)
    case _ => BigInt(0)
).reduce(_+_)

println("Part 1 checksum: " + checksum(diskBlocks))


val diskBlocksPart2 = parseDiskMap(0, diskMap, 0).toArray

val fileSections = fileBlocks(diskBlocksPart2).groupBy((block, i) => {
    block.asInstanceOf[File].id
})

val findFreeSpan = (size: Int, before: Int) => diskBlocksPart2.zipWithIndex.indexWhere((block, bi) => 
    bi < before && List.range(0, size).forall(i => i < diskBlocksPart2.size && diskBlocksPart2.apply(bi + i).isInstanceOf[Free])
)

fileSections.keys.toList.sorted.reverse.foreach(key => {
    if(debug) println(diskBlocksPart2.mkString)
    val fileSection = fileSections(key)
    if (debug) print("find new home for " + key + ": size " + fileSection.size )
    val freeSpanIndex = findFreeSpan(fileSection.size, fileSection.apply(0)._2)
    if (freeSpanIndex >= 0) {
        if (debug)  println(" - free span starting at " + freeSpanIndex)
        fileSection.zipWithIndex.foreach((filesection, counter) => {
            val (fileblock, fileblockIndex) = filesection
            val freeblock = diskBlocksPart2.apply(freeSpanIndex + counter)
            diskBlocksPart2(fileblockIndex) = freeblock
            diskBlocksPart2(freeSpanIndex + counter) = fileblock
        })

    } else if (debug) println(" - No span available ")
    if (debug) println()
})

println("Part 2 checksum: " + checksum(diskBlocksPart2))