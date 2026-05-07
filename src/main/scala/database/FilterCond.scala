package database

import scala.language.implicitConversions

// 2.2.1
trait FilterCond {
  def eval(r: Row): Option[Boolean]
}

// look up colName in the row; if found, apply predicate
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    val valueOpt = r.get(colName)
    valueOpt.map(value => predicate(value))
}

// recursive fold: eval head, eval Compound on tail, combine with op
case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    if conditions.isEmpty then None
    else if conditions.tail.isEmpty then conditions.head.eval(r)
    else
      val head = conditions.head
      val tail = conditions.tail
      val firstResult = head.eval(r)

      firstResult.flatMap(b1 =>
        val restResult = Compound(op, tail).eval(r)
        restResult.map(b2 => op(b1, b2))
      )
}

// just flip the inner result
case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    f.eval(r).map(b => !b)
}

// build Compound with the right binary op
def And(f1: FilterCond, f2: FilterCond): FilterCond =
  Compound((a, b) => a && b, List(f1, f2))

def Or(f1: FilterCond, f2: FilterCond): FilterCond =
  Compound((a, b) => a || b, List(f1, f2))

def Equal(f1: FilterCond, f2: FilterCond): FilterCond =
  Compound((a, b) => a == b, List(f1, f2))

// flatMap eval over fs to drop Nones; .exists(identity) for at least one true
case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    val results = fs.flatMap(f => f.eval(r))
    if results.isEmpty then None
    else Some(results.exists(b => b))
}

// same idea, .forall(identity) for all true
case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    val results = fs.flatMap(f => f.eval(r))
    if results.isEmpty then None
    else Some(results.forall(b => b))
}

// 2.2.2
// syntactic sugar; just delegates to And/Or/Equal/Not
extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond)  = And(f, other)
  def ||(other: FilterCond)  = Or(f, other)
  def unary_!                = Not(f)
}

// 2.2.3
// (col, predicate) tuple becomes a Field automatically
implicit def tuple2Field(t: (String, String => Boolean)): Field =
  Field(t._1, t._2)

extension(t: Table) {

  // 2.2.4
  // keep rows where eval returns Some(true); None treated as false
  def filter(f: FilterCond): Table =
    val keptRows = t.tableData.filter(row =>
      val result = f.eval(row)
      result.getOrElse(false)
    )
    Table(t.tableName, keptRows)

  // 2.2.5
  // map over rows: if condition holds, merge updates with ++ (overwrites)
  def update(f: FilterCond, updates: Map[String, String]): Table =
    val newRows = t.tableData.map(row =>
      val matches = f.eval(row).getOrElse(false)
      if matches then row ++ updates else row
    )
    Table(t.tableName, newRows)
}
