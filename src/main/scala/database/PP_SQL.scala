package database

import scala.language.implicitConversions

// 2.3.1
// each case class wraps a table + params; eval just delegates to the matching Table method
trait PP_SQL_Table {
  def eval: Option[Table]
}

// foldLeft over values, calling insert for each
case class InsertRow(table: Table, values: Tabular) extends PP_SQL_Table {
  def eval: Option[Table] =
    val result = values.foldLeft(table)((acc, row) => acc.insert(row))
    Some(result)
}

case class SortTable(table: Table, column: String) extends PP_SQL_Table {
  def eval: Option[Table] =
    Some(table.sort(column))
}

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table {
  def eval: Option[Table] =
    Some(table.update(condition, updates))
}

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table {
  def eval: Option[Table] =
    Some(table.delete(row))
}

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table {
  def eval: Option[Table] =
    Some(table.filter(condition))
}

case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table {
  def eval: Option[Table] =
    Some(table.select(columns))
}

// 2.3.2
// each implicit unwraps the Option[Table] (propagating None) and builds the matching case class
implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] =
  val tableOpt = t._1
  val values   = t._3
  tableOpt.map(table => InsertRow(table, values))

implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] =
  val tableOpt = t._1
  val column   = t._3
  tableOpt.map(table => SortTable(table, column))

implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] =
  val tableOpt  = t._1
  val condition = t._3
  val updates   = t._4
  tableOpt.map(table => UpdateRow(table, condition, updates))

implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] =
  val tableOpt = t._1
  val row      = t._3
  tableOpt.map(table => DeleteRow(table, row))

implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] =
  val tableOpt  = t._1
  val condition = t._3
  tableOpt.map(table => FilterRows(table, condition))

implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] =
  val tableOpt = t._1
  val columns  = t._3
  tableOpt.map(table => SelectColumns(table, columns))

// unwrap the Option and call eval on the query
def queryT(p: Option[PP_SQL_Table]): Option[Table] =
  p match {
    case Some(pp) => pp.eval
    case _        => None
  }
