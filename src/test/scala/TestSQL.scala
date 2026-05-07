import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpec
import database.*

class TestSQL extends AnyFunSpec with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    //30p
    println(s"Punctaj SQL: ${SharedTestData.sqlScore}p/30p")
  }

  describe("TDA Table") {
    it("InsertRow (2p)") {
      val insertRow: PP_SQL_Table = InsertRow(Utils.people,
        List(Map("id" -> "11", "name" -> "Jack", "age" -> "33", "address" -> "181 Pine St")))
      val res: Table = Utils.people.insert(
        Map("id" -> "11", "name" -> "Jack", "age" -> "33", "address" -> "181 Pine St"))
      assert(insertRow.eval.get.toString == res.toString)
      SharedTestData.addPointsSQL(2)
    }
    it("UpdateRow (2p)") {
      val row: Row = Map("id" -> "4000", "name" -> "Julia123", "age" -> "125", "address" -> "101 Oak St")
      val updateRow: PP_SQL_Table = UpdateRow(Utils.people, Field("name", _ == "Jill"), row)
      val res: Table = Utils.people.update(Field("name", _ == "Jill"), row)
      assert(updateRow.eval.get.toString == res.toString)
      SharedTestData.addPointsSQL(2)
    }
    it("DeleteRow (2p)") {
      val row: Row = Map("id" -> "5", "name" -> "Jack", "hobby" -> "Clay modeling")
      val deleteRow: PP_SQL_Table = DeleteRow(Utils.hobbies, row)
      val res: Table = Utils.hobbies.delete(row)
      assert(deleteRow.eval.get.toString == res.toString)
      SharedTestData.addPointsSQL(2)
    }
    it("SortTable (2p)") {
      val sortTable: PP_SQL_Table = SortTable(Utils.people, "name")
      val res: Table = Utils.people.sort("name")
      assert(sortTable.eval.get.toString == res.toString)
      SharedTestData.addPointsSQL(2)
    }
    it("FilterRows (2p)") {
      val filter: FilterCond = Not(Field("id", _ == "9"))
      val filterTable: PP_SQL_Table = FilterRows(Utils.people, filter)
      val res: Table = Utils.people.filter(filter)
      assert(filterTable.eval.get.toString == res.toString)
      SharedTestData.addPointsSQL(2)
    }
  }

  describe("implicits") {
    it("Test implicits (4p)") {
      val sql1 : Option[Table] = queryT((Some(Utils.people), "EXTRACT", List("id", "name")))
      val sql2 = PP_SQL_Table_Select((Some(Utils.people), "EXTRACT", List("id", "name")))
      assert(sql1.get.toString == sql2.get.eval.get.toString)
      SharedTestData.addPointsSQL(4)
    }
  }

  describe("prettier language") {
    it("INSERT (4p)") {
      val sql: Option[Table] = queryT((Some(Utils.people), "INSERT",
        List(Map(
            "id" -> "10",
            "name" -> "Johnnatan",
            "age" -> "93",
            "address" -> "123 Main St"))))
      val ref: String = """id,name,age,address
                          |1,John,23,123 Main St
                          |2,Jane,27,456 Elm St
                          |3,Joe,30,789 Maple St
                          |4,Jill,25,101 Oak St
                          |5,Jack,27,112 Pine St
                          |6,Jen,24,131 Cedar St
                          |7,Jim,26,141 Birch St
                          |8,Jesse,29,151 Spruce St
                          |9,Jenny,23,161 Fir St
                          |10,Jerry,28,171 Larch St
                          |10,Johnnatan,93,123 Main St""".stripMargin
      assert(sql.get.toString == ref)
      SharedTestData.addPointsSQL(4)
    }
    it("SORT (4p)") {
      val sql: Option[Table] =  queryT((Some(Utils.hobbies), "SORT", "hobby"))
      val ref: String = """id,name,hobby
                          |1,John,Basketball
                          |5,Jack,Clay modeling
                          |10,Jerry,Dancing
                          |6,Jen,Film making
                          |3,Joe,Football
                          |9,Jenny,Makeup
                          |8,Mimi,Music
                          |4,Mona,Painting
                          |2,Jane,Skiing
                          |7,Maria,Skin care""".stripMargin
      assert(sql.get.toString == ref)
      SharedTestData.addPointsSQL(4)
    }
    it("DELETE (4p)") {
      val sql: Option[Table] = queryT((Some(Utils.people), "DELETE",
        Map("id" -> "6", "name" -> "Jen", "age" -> "24", "address" -> "131 Cedar St")))
      val ref: String = """id,name,age,address
          |1,John,23,123 Main St
          |2,Jane,27,456 Elm St
          |3,Joe,30,789 Maple St
          |4,Jill,25,101 Oak St
          |5,Jack,27,112 Pine St
          |7,Jim,26,141 Birch St
          |8,Jesse,29,151 Spruce St
          |9,Jenny,23,161 Fir St
          |10,Jerry,28,171 Larch St""".stripMargin
      assert(sql.get.toString == ref)
      SharedTestData.addPointsSQL(4)
    }
    it("UPDATE (4p)") {
      val sql: Option[Table] =
        queryT((Some(Utils.people), "UPDATE",
          Field("name", _ == "Jenny"),
          Map("name" -> "weeeeeee")
        ))
      val ref: String = """id,name,age,address
                          |1,John,23,123 Main St
                          |2,Jane,27,456 Elm St
                          |3,Joe,30,789 Maple St
                          |4,Jill,25,101 Oak St
                          |5,Jack,27,112 Pine St
                          |6,Jen,24,131 Cedar St
                          |7,Jim,26,141 Birch St
                          |8,Jesse,29,151 Spruce St
                          |9,weeeeeee,23,161 Fir St
                          |10,Jerry,28,171 Larch St""".stripMargin
      assert(sql.get.toString == ref)
      SharedTestData.addPointsSQL(4)
    }
    it("EXTRACT (4p)") {
      val sql: Option[Table] = queryT((Some(Utils.people), "EXTRACT", List("id", "name")))
      val ref: String = """id,name
                          |1,John
                          |2,Jane
                          |3,Joe
                          |4,Jill
                          |5,Jack
                          |6,Jen
                          |7,Jim
                          |8,Jesse
                          |9,Jenny
                          |10,Jerry""".stripMargin
      assert(sql.get.toString == ref)

    }
    it("FILTER (4p)") {
      val sql: Option[Table] = queryT((Some(Utils.people), "FILTER", Not(Field("id", _ == "9"))))
      val ref: String = """id,name,age,address
                          |1,John,23,123 Main St
                          |2,Jane,27,456 Elm St
                          |3,Joe,30,789 Maple St
                          |4,Jill,25,101 Oak St
                          |5,Jack,27,112 Pine St
                          |6,Jen,24,131 Cedar St
                          |7,Jim,26,141 Birch St
                          |8,Jesse,29,151 Spruce St
                          |10,Jerry,28,171 Larch St""".stripMargin
      assert(sql.get.toString == ref)
    }
  }

}
