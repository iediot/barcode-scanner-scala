import decoder.Reader.readBarcodes
// Ctrl + Shift + P -> Metals: Import Build, Metals: Restart Build Server
import com.github.tototoshi.csv._
import database.Row
import database.Table
import shop.Commands
def readCsv(path: String): List[Row] = {
  val reader = CSVReader.open(path)
  try {
    val all = reader.all()
    val header = all.head
    all.tail.map(values => header.zip(values).toMap)
  } finally {
    reader.close()
  }
}

def buildProductTable(csvRows: List[Row], barcodes: List[(String, String)]): Table = {
  val barcodeMap = barcodes.toMap

  val finalRows = for {
    row <- csvRows
    imageName <- row.get("ImageName")
    barcode <- barcodeMap.get(imageName)
  } yield 
        Map(
          "Barcode" -> barcode,
          "Name"    -> row.getOrElse("Name", ""),
          "Price"   -> row.getOrElse("Price", "")
        )

  Table("Products", finalRows)
}
@main def main(): Unit = {
  val barcodes = readBarcodes("MyBarcodesInput", "MyBarcodesOutput")
  barcodes.foreach((fname, barcode) => println(fname + ": " + barcode))

  val productTable = buildProductTable(readCsv("data.csv"), barcodes)

  val commands = new Commands(productTable)
  val shoppingList = commands.START_SHOPPING()
  println("Shopping List: " + commands.ADD_PRODUCT(shoppingList, "5203149006215", 2))
}
