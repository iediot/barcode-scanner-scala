package database

import scala.language.implicitConversions

// 2.3.1
// each case class wraps a table + params; eval just delegates to the matching Table method
trait PP_SQL_Table{
  def eval: Option[Table]
}

// foldLeft over values, calling insert for each
case class InsertRow(table:Table, values: Tabular) extends PP_SQL_Table{
  def eval: Option[Table] = Some(values.foldLeft(table)(_.insert(_)))
}

case class SortTable(table: Table, column: String) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.sort(column))
}

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.update(condition, updates))
}

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.delete(row))
}

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.filter(condition))
}

case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.select(columns))
}

// 2.3.2
// each implicit unwraps the Option[Table] (propagating None) and builds the matching case class
implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] =
  t._1.map(table => InsertRow(table, t._3))

implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] =
  t._1.map(table => SortTable(table, t._3))

implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] =
  t._1.map(table => UpdateRow(table, t._3, t._4))

implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] =
  t._1.map(table => DeleteRow(table, t._3))

implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] =
  t._1.map(table => FilterRows(table, t._3))

implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] =
  t._1.map(table => SelectColumns(table, t._3))


// unwrap the Option and call eval on the query
def queryT(p: Option[PP_SQL_Table]): Option[Table] = p match {
  case Some(pp) => pp.eval
  case _ => None
}
