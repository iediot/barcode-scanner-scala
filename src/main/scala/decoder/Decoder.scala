package decoder

import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable

object Decoder {

  // TODO 1.1.1
  // pattern-match the input, return the matching Bit
  given char2Bit: Conversion[Char, Bit] with
    def apply(x: Char): Bit =
      if x == '1' then One else Zero

  given int2Bit: Conversion[Int, Bit] with
    def apply(s: Int): Bit =
      if s == 1 then One else Zero

  // TODO 1.1.2
  // simple 2-case match/if
  extension(c: Bit)
    def complement: Bit =
      if c == One then Zero else One

  // TODO 1.1.3
  // L: parse the strings into Bits
  // R: derive from L (per-bit transform)
  // G: derive from R (list-level transform)
  val LStrings: List[String] = List(
    "0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011"
  )

  val leftOddList: List[List[Bit]] =
    LStrings.map(s => s.toList.map(c => char2Bit(c)))

  val rightList: List[List[Bit]] =
    leftOddList.map(bits => bits.map(b => b.complement))

  val leftEvenList: List[List[Bit]] =
    rightList.map(bits => bits.reverse)

  // TODO 1.1.4
  // recursion: peel off head, span on tail to grab equals,
  // prepend the group, recurse on the leftover
  extension[A](l: List[A])
    def groupedByEquality: List[List[A]] =
      if l.isEmpty then Nil
      else
        val head = l.head
        val tail = l.tail
        val (same, rest) = tail.span(x => x == head)
        val firstGroup = head :: same
        firstGroup :: rest.groupedByEquality

  // TODO 1.1.5
  // reuse 1.1.4, then map each group to (size, value)
  def runLength[A](l: List[A]): List[(Int, A)] =
    val groups = l.groupedByEquality
    groups.map(group => (group.length, group.head))

  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt =>
        this.a.abs == that.a.abs &&
          this.b.abs == that.b.abs &&
          this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 1.2.1
    // standard fraction formulas, build a new RatioInt
    // (no need to simplify — the constructor does it)
    def -(other: RatioInt): RatioInt =
      RatioInt(a * other.b - other.a * b, b * other.b)

    def +(other: RatioInt): RatioInt =
      RatioInt(a * other.b + other.a * b, b * other.b)

    def *(other: RatioInt): RatioInt =
      RatioInt(a * other.a, b * other.b)

    def /(other: RatioInt): RatioInt =
      RatioInt(a * other.b, b * other.a)

    // TODO 1.2.2
    // compute the sign of (a*d - c*b); widen to Long first
    def compare(other: RatioInt): Int =
      val diff = a.toLong * other.b - other.a.toLong * b
      if diff < 0 then -1
      else if diff > 0 then 1
      else 0
  }

  // TODO 1.3.1
  // get the total, then rescale each count over it
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] =
    val total = l.map(pair => pair._1).sum
    l.map(pair =>
      val count = pair._1
      val elem  = pair._2
      (RatioInt(count, total), elem)
    )

  // TODO 1.3.2
  // grab the head's bit, scale the list, drop the bit half
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) =
    val firstBit = l.head._2
    val scaled   = scaleToOne(l)
    val widths   = scaled.map(pair => pair._1)
    (firstBit, widths)

  // TODO 1.3.3
  // map char-by-char, one case for G, default Odd
  def toParities(s: Str): List[Parity] =
    s.map(c => if c == 'G' then Even else Odd)

  // TODO 1.3.4
  // map PStrings through 1.3.3
  val PStrings: List[String] = List(
    "LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL"
  )
  val leftParityList: List[List[Parity]] =
    PStrings.map(s => toParities(s.toList))

  // TODO 1.3.5
  // pipeline: runLength then scaledRunLength on each list
  type SRL = (Bit, List[RatioInt])

  val leftOddSRL: List[SRL] =
    leftOddList.map(bits => scaledRunLength(runLength(bits)))

  val leftEvenSRL: List[SRL] =
    leftEvenList.map(bits => scaledRunLength(runLength(bits)))

  val rightSRL: List[SRL] =
    rightList.map(bits => scaledRunLength(runLength(bits)))

  // TODO 1.4.1
  // mismatch on first bit -> sentinel "infinity"
  // else zip widths, take abs of each diff, sum them up
  def distance(l1: SRL, l2: SRL): RatioInt =
    val firstBit1 = l1._1
    val firstBit2 = l2._1
    if firstBit1 != firstBit2 then
      RatioInt(100, 1)
    else
      val widths1 = l1._2
      val widths2 = l2._2
      val pairs   = widths1.zip(widths2)

      val absDiffs = pairs.map(pair =>
        val w1   = pair._1
        val w2   = pair._2
        val diff = w1 - w2
        if diff.a < 0 then RatioInt(-diff.a, diff.b)
        else diff
      )

      absDiffs.foldLeft(RatioInt(0, 1))((sum, x) => sum + x)

  // TODO 1.4.2
  // attach indices, compute distances, take the minimum
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) =
    val withIndex     = SRL_Codes.zipWithIndex
    val withDistances = withIndex.map(pair =>
      val code = pair._1
      val idx  = pair._2
      (distance(code, digitCode), idx)
    )
    withDistances.minBy(pair => pair._1)

  // TODO 1.4.3
  // try both tables, keep the closer match, return its parity too
  def bestLeft(digitCode: SRL): (Parity, Digit) =
    val resultOdd  = bestMatch(leftOddSRL,  digitCode)
    val resultEven = bestMatch(leftEvenSRL, digitCode)
    val distOdd  = resultOdd._1
    val digitOdd = resultOdd._2
    val distEven  = resultEven._1
    val digitEven = resultEven._2
    if distOdd <= distEven then (Odd, digitOdd)
    else (Even, digitEven)

  // TODO 1.4.4
  // single-table match, fixed parity
  def bestRight(digitCode: SRL): (Parity, Digit) =
    val result = bestMatch(rightSRL, digitCode)
    val digit  = result._2
    (NoParity, digit)

  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] = {
    def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
      l match {
        case Nil => Nil
        case _ =>
          val (h, t) = f(l)
          h :: chunkWith(f)(t)
      }
    }
    chunkWith((l: List[A]) => l.splitAt(n))(l)
  }

  // TODO 1.4.5
  // length check; slice off start/middle/end markers;
  // chunk each side by 4, decode each chunk, concat
  def findLast12Digits(rle: List[(Int, Bit)]): List[(Parity, Digit)] =
    if rle.length != 59 then Nil
    else
      val leftBars  = rle.drop(3).take(24)
      val rightBars = rle.drop(32).take(24)

      val leftChunks  = chunksOf(4)(leftBars)
      val rightChunks = chunksOf(4)(rightBars)

      val leftDigits  = leftChunks.map(chunk  => bestLeft(scaledRunLength(chunk)))
      val rightDigits = rightChunks.map(chunk => bestRight(scaledRunLength(chunk)))

      leftDigits ++ rightDigits

  // TODO 1.4.6
  // pull the parities, look them up in the parity table, return the index
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] =
    val first6   = l.take(6)
    val parities = first6.map(pair => pair._1)
    val indexed  = leftParityList.zipWithIndex
    val found    = indexed.find(pair => pair._1 == parities)
    found.map(pair => pair._2)

  // TODO 1.4.7
  // weighted sum (1,3,1,3,...), then formula
  def checkDigit(l: List[Digit]): Digit =
    val weights = List(1,3,1,3,1,3,1,3,1,3,1,3)
    val pairs   = l.zip(weights)
    val sum     = pairs.map(pair => pair._1 * pair._2).sum
    (10 - (sum % 10)) % 10

  // TODO 1.4.8
  // length must be 13; recompute check digit from first 12, compare with last
  def verifyCode(code: List[(Parity, Digit)]): Option[String] =
    if code.length != 13 then None
    else
      val digits   = code.map(pair => pair._2)
      val first12  = digits.take(12)
      val expected = checkDigit(first12)
      val actual   = digits(12)
      if expected == actual then Some(digits.mkString)
      else None

  // TODO 1.4.9
  // 1.4.5 -> 1.4.6 -> prepend the first -> 1.4.8
  def solve(rle: List[(Int, Bit)]): Option[String] =
    val last12 = findLast12Digits(rle)
    val firstOpt = firstDigit(last12)
    firstOpt match
      case None => None
      case Some(first) =>
        val full = (NoParity, first) :: last12
        verifyCode(full)

  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row)

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, pair._2)))
  }
}
