package shop

import database.Table
import database.queryT
import database.Field
import database.Not
import database.PP_SQL_Table_Filter
import database.PP_SQL_Table_Update
import database.PP_SQL_Table_Insert
import database.update
import database.filter

class Commands(productsTable: Table) {

    // 3.1.1
    // empty cart, just an empty Tabular
    def START_SHOPPING(): Table =
        Table("ShoppingCart", Nil)

    // 3.1.2
    // find product in productsTable by Barcode
    // if already in cart -> update quantity (oldQty + quantity)
    // else -> insert new row with name/quantity/price
    def ADD_PRODUCT(shopList: Table, barcode: String, quantity: Int): Table = {
        val productOpt = productsTable.tableData.find(row =>
            row.get("Barcode").contains(barcode)
        )

        productOpt match {
            case None => shopList
            case Some(product) =>
                val name  = product.getOrElse("Name", "")
                val price = product.getOrElse("Price", "")

                val existing = shopList.tableData.find(row =>
                    row.get("name").contains(name)
                )

                existing match {
                    case Some(row) =>
                        val oldQtyStr = row.getOrElse("quantity", "0")
                        val oldQty    = oldQtyStr.toIntOption.getOrElse(0)
                        val newQty    = (oldQty + quantity).toString

                        val condition = Field("name", n => n == name)
                        val updates   = Map("quantity" -> newQty)
                        shopList.update(condition, updates)

                    case None =>
                        val newRow = Map(
                            "name"     -> name,
                            "quantity" -> quantity.toString,
                            "price"    -> price
                        )
                        shopList.insert(newRow)
                }
        }
    }

    // 3.1.3
    // FILTER query keeping rows where name != target
    def DELETE_PRODUCT(t: Table, name: String): Table =
        val condition = Field("name", n => n != name)
        val query     = PP_SQL_Table_Filter((Some(t), "FILTER", condition))
        queryT(query).getOrElse(t)

    // 3.1.4
    // UPDATE query setting quantity on rows matching name
    def EDIT_QUANTITY(t: Table, name: String, newQuantity: Int): Table =
        val condition = Field("name", n => n == name)
        val updates   = Map("quantity" -> newQuantity.toString)
        val query     = PP_SQL_Table_Update((Some(t), "UPDATE", condition, updates))
        queryT(query).getOrElse(t)
}
