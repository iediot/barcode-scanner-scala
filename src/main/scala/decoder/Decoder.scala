package decoder

import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable

object Decoder {
  // TODO 1.1.1
  // pattern match: '1'/1 -> One, else Zero
  given char2Bit: Conversion[Char, Bit] with
    def apply(x: Char): Bit = if x == '1' then One else Zero
  given int2Bit: Conversion[Int, Bit] with
    def apply(s: Int): Bit = if s == 1 then One else Zero

  // TODO 1.1.2
  // match the bit, return the opposite
  extension(c:Bit)
    def complement: Bit = c match
      case One  => Zero
      case Zero => One

  // TODO 1.1.3
  // map LStrings -> char2Bit per char to get leftOdd
  // right = map complement over each L encoding
  // leftEven = map reverse over each R encoding
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
  val leftOddList: List[List[Bit]] = LStrings.map(s => s.toList.map(c => char2Bit(c)))
  val rightList: List[List[Bit]] = leftOddList.map(bits => bits.map(_.complement))
  val leftEvenList: List[List[Bit]] = rightList.map(bits => bits.reverse)

  // TODO 1.1.4
  // recurse: split tail with span(_ == head), prepend (head :: same), recurse on rest
  extension[A](l: List[A])
    def groupedByEquality: List[List[A]] = l match
      case Nil => Nil
      case head :: tail =>
        val (same, rest) = tail.span(_ == head)
        (head :: same) :: rest.groupedByEquality

  // TODO 1.1.5
  // groupedByEquality then map each group to (length, head)
  def runLength[A](l: List[A]): List[(Int, A)] =
    l.groupedByEquality.map(group => (group.length, group.head))

  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }
    // TODO 1.2.1
    // cross-multiply: a/b op c/d. constructor reduces by gcd
    def -(other: RatioInt): RatioInt = RatioInt(a * other.b - other.a * b, b * other.b)
    def +(other: RatioInt): RatioInt = RatioInt(a * other.b + other.a * b, b * other.b)
    def *(other: RatioInt): RatioInt = RatioInt(a * other.a, b * other.b)
    def /(other: RatioInt): RatioInt = RatioInt(a * other.b, b * other.a)

    // TODO 1.2.2
    // sign(a*d - c*b), cast to Long to avoid Int overflow
    def compare(other: RatioInt): Int =
      val diff = a.toLong * other.b - other.a.toLong * b
      if diff < 0 then -1 else if diff > 0 then 1 else 0
  }

  // TODO 1.3.1
  // sum all counts, divide each by total -> RatioInt
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] =
    val total = l.map(_._1).sum
    l.map((count, elem) => (RatioInt(count, total), elem))

  // TODO 1.3.2
  // grab head bit, scaleToOne, drop the bit half from each pair
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) =
    val firstBit = l.head._2
    val scaled = scaleToOne(l)
    (firstBit, scaled.map(_._1))

  // TODO 1.3.3
  // map: 'G' -> Even, anything else -> Odd
  def toParities(s: Str): List[Parity] =
    s.map(c => if c == 'G' then Even else Odd)

  // TODO 1.3.4
  // map PStrings through toParities
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = PStrings.map(s => toParities(s.toList))

  // TODO 1.3.5
  // for each encoding: runLength then scaledRunLength
  type SRL = (Bit, List[RatioInt])
  val leftOddSRL:  List[SRL] = leftOddList.map(bits => scaledRunLength(runLength(bits)))
  val leftEvenSRL: List[SRL] = leftEvenList.map(bits => scaledRunLength(runLength(bits)))
  val rightSRL:    List[SRL] = rightList.map(bits => scaledRunLength(runLength(bits)))

  // TODO 1.4.1
  // first bits differ -> RatioInt(100, 1) (infinity sentinel)
  // else zip widths, sum |w1 - w2| with foldLeft
  def distance(l1: SRL, l2: SRL): RatioInt =
    if l1._1 != l2._1 then RatioInt(100, 1)
    else
      l1._2.zip(l2._2).map((r1, r2) =>
        val diff = r1 - r2
        if diff.a < 0 then RatioInt(-diff.a, diff.b) else diff
      ).foldLeft(RatioInt(0, 1))(_ + _)

  // TODO 1.4.2
  // zipWithIndex, map to (distance, idx), minBy distance
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) =
    SRL_Codes.zipWithIndex
      .map((code, idx) => (distance(code, digitCode), idx))
      .minBy(_._1)

  // TODO 1.4.3
  // bestMatch on both odd and even tables, pick the smaller distance
  // return its parity (Odd or Even) too
  def bestLeft(digitCode: SRL): (Parity, Digit) =
    val (distOdd, digitOdd) = bestMatch(leftOddSRL, digitCode)
    val (distEven, digitEven) = bestMatch(leftEvenSRL, digitCode)
    if distOdd <= distEven then (Odd, digitOdd) else (Even, digitEven)

  // TODO 1.4.4
  // bestMatch on rightSRL, parity is always NoParity
  def bestRight(digitCode: SRL): (Parity, Digit) =
    val (_, digit) = bestMatch(rightSRL, digitCode)
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
  // length must be 59. drop 3 start, take 24 left bars; drop 3+24+5, take 24 right
  // chunksOf(4) each side, scaledRunLength each chunk, bestLeft / bestRight
  def findLast12Digits(rle: List[(Int, Bit)]): List[(Parity, Digit)] =
    if rle.length != 59 then Nil
    else
      val leftBars  = rle.drop(3).take(24)
      val rightBars = rle.drop(3 + 24 + 5).take(24)
      val leftDigits  = chunksOf(4)(leftBars).map(chunk => bestLeft(scaledRunLength(chunk)))
      val rightDigits = chunksOf(4)(rightBars).map(chunk => bestRight(scaledRunLength(chunk)))
      leftDigits ++ rightDigits

  // TODO 1.4.6
  // take first 6 parities, find them in leftParityList, return the index
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] =
    val parities = l.take(6).map(_._1)
    leftParityList.zipWithIndex.find((pList, _) => pList == parities).map(_._2)

  // TODO 1.4.7
  // weights = [1,3,1,3,...]; sum digit*weight; (10 - sum%10) % 10
  def checkDigit(l: List[Digit]): Digit =
    val weights = List(1,3,1,3,1,3,1,3,1,3,1,3)
    val sum = l.zip(weights).map((d, w) => d * w).sum
    (10 - (sum % 10)) % 10

  // TODO 1.4.8
  // require 13 elements; checkDigit on first 12, compare with digit 13
  def verifyCode(code: List[(Parity, Digit)]): Option[String] =
    if code.length != 13 then None
    else
      val digits = code.map(_._2)
      val check = checkDigit(digits.take(12))
      if check == digits(12) then Some(digits.mkString)
      else None

  // TODO 1.4.9
  // findLast12Digits -> firstDigit -> prepend (NoParity, first) -> verifyCode
  def solve(rle: List[(Int, Bit)]): Option[String] =
    val last12 = findLast12Digits(rle)
    firstDigit(last12) match
      case None => None
      case Some(first) =>
        val full = (NoParity, first) :: last12
        verifyCode(full)

  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

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
