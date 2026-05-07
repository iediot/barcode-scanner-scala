package database

import scala.annotation.tailrec

type Row = Map[String, String]
type Tabular = List[Row]

object Table {
  // 2.1.1
  // split by \n, trim, drop empty lines
  // first line = header (split by ,); remaining lines zip values with header into a Map
  def apply(name: String, s: String): Table = {
    val rawLines = s.split("\n").map(line => line.trim)
    val lines    = rawLines.filter(line => line.nonEmpty).toList

    if lines.isEmpty then Table(name, Nil)
    else
      val headerLine = lines.head
      val header     = headerLine.split(",").map(col => col.trim).toList

      val rows = lines.tail.map(line =>
        val values = line.split(",", -1).map(v => v.trim).toList
        header.zip(values).toMap
      )

      Table(name, rows)
  }
}

case class Table(tableName: String, tableData: Tabular) {

  def header: List[String] =
    tableData.headOption.map(row => row.keys.toList).getOrElse(Nil)

  def data: Tabular = tableData
  def name: String  = tableName

  // 2.1.2
  // header joined by , then each row's values in header order; lines joined by \n
  override def toString: String =
    if tableData.isEmpty then ""
    else
      val h = header

      val headerLine = h.mkString(",")
      val rowLines   = tableData.map(row =>
        val cells = h.map(col => row.getOrElse(col, ""))
        cells.mkString(",")
      )

      (headerLine :: rowLines).mkString("\n")

  // 2.1.3
  // skip if duplicate, else append with :+
  def insert(row: Row): Table =
    if tableData.contains(row) then this
    else Table(tableName, tableData :+ row)

  // 2.1.4
  // filter out rows equal to the given one
  def delete(row: Row): Table =
    val newData = tableData.filter(r => r != row)
    Table(tableName, newData)

  // 2.1.5
  // sortBy the value at the given column; reverse if descending
  def sort(column: String, ascending: Boolean = true): Table =
    val sorted = tableData.sortBy(row => row.getOrElse(column, ""))
    val finalData = if ascending then sorted else sorted.reverse
    Table(tableName, finalData)

  // 2.1.6
  // for each row keep only listed columns; flatMap drops missing ones
  def select(columns: List[String]): Table =
    val newData = tableData.map(row =>
      val keptPairs = columns.flatMap(col =>
        row.get(col).map(value => col -> value)
      )
      keptPairs.toMap
    )
    Table(tableName, newData)
}

extension (table: Table) {
  def apply(i: Int): Row = {
    table.data(i)
  }
}
