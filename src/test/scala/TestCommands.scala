import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpec
import database.*
import Utils.*
import shop.*

class TestCommands extends AnyFunSpec with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    //20p
    println(s"Punctaj Shop: ${SharedTestData.shopScore}p/20p")
  }

    describe("Commands") {
        it("Add (5p)") {
        val productTable = Utils.productTable
        val commands = new Commands(productTable)
        val shoppingList = commands.START_SHOPPING()
        val updatedList = commands.ADD_PRODUCT(shoppingList, "123", 2)
        val updatedList2 = commands.ADD_PRODUCT(updatedList, "456", 3)
        assert(updatedList.tableData.exists(row => row("name") == "Product1" && row("quantity") == "2"))
        assert(updatedList2.tableData.exists(row => row("name") == "Product2" && row("quantity") == "3"))
        SharedTestData.addPointsShop(5)
        }

        it("Edit (5p)") {
        // use product table from uitls.scala
        val productTable = Utils.productTable
        val commands = new Commands(productTable)
        val shoppingList = commands.START_SHOPPING()
        val updatedList = commands.ADD_PRODUCT(shoppingList, "123", 2)
        val editedList = commands.EDIT_QUANTITY(updatedList, "Product1", 5)
        assert(editedList.tableData.exists(row => row("name") == "Product1" && row("quantity") == "5"))
        SharedTestData.addPointsShop(10)
        }

        it("Remove (5p)") {
        val productTable = Utils.productTable
        val commands = new Commands(productTable)
        val shoppingList = commands.START_SHOPPING()
        val updatedList = commands.ADD_PRODUCT(shoppingList, "123", 2)
        val removedList = commands.DELETE_PRODUCT(updatedList, "Product1")
        assert(!removedList.tableData.exists(row => row("name") == "Product1"))
        SharedTestData.addPointsShop(5)
        }
    }
}