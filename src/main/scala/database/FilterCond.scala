package database

import scala.language.implicitConversions

// 2.2.1
trait FilterCond {def eval(r: Row): Option[Boolean]}

// look up colName in the row; if found, apply predicate
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = r.get(colName).map(predicate)
}

// recursive fold: eval head, eval Compound on tail, combine with op
case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = conditions match
    case Nil => None
    case head :: Nil => head.eval(r)
    case head :: tail =>
      head.eval(r).flatMap(b1 =>
        Compound(op, tail).eval(r).map(b2 => op(b1, b2))
      )
}

// just flip the inner result
case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = f.eval(r).map(!_)
}

// build Compound with the right binary op
def And(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ && _, List(f1, f2))
def Or(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ || _, List(f1, f2))
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ == _, List(f1, f2))

// flatMap eval over fs to drop Nones; .exists(identity) for at least one true
case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    val results = fs.flatMap(_.eval(r))
    if results.isEmpty then None else Some(results.exists(identity))
}

// same idea, .forall(identity) for all true
case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    val results = fs.flatMap(_.eval(r))
    if results.isEmpty then None else Some(results.forall(identity))
}

// 2.2.2
// syntactic sugar; just delegates to And/Or/Equal/Not
extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def unary_! = Not(f)
}

// 2.2.3
// (col, predicate) tuple becomes a Field automatically
implicit def tuple2Field(t: (String, String => Boolean)): Field = Field(t._1, t._2)

extension(t: Table) {
  // 2.2.4
  // keep rows where eval returns Some(true); None treated as false
  def filter(f: FilterCond): Table =
    Table(t.tableName, t.tableData.filter(row => f.eval(row).getOrElse(false)))

  // 2.2.5
  // map over rows: if condition holds, merge updates with ++ (overwrites)
  def update(f: FilterCond, updates: Map[String, String]): Table =
    Table(t.tableName, t.tableData.map(row =>
      if f.eval(row).getOrElse(false) then row ++ updates else row
    ))
}
