import org.scalatest.Reporter
import org.scalatest.events._

class CustomReporter extends Reporter {

  private var total = 0
  private var passed = 0
  private var failed = 0

  override def apply(event: Event): Unit = event match {
    case _: TestSucceeded =>
      total += 1
      passed += 1

    case _: TestFailed =>
      total += 1
      failed += 1

    case _: TestIgnored =>
      total += 1

    case _: RunCompleted =>
      println(s"\n===== REZULTAT FINAL =====")
      println(s"Passed: $passed/$total")
      if(failed > 0) {
        println(s"Failed: $failed")
      }
      println(s"Total: ${SharedTestData.punctajTotal/2}/100  (${SharedTestData.punctajTotal}p/200p)")
      if(SharedTestData.punctajTotal == 200) {
        println("Felicitari! Ai rezolvat toata tema. Asteptam sa ne spui cum ai facut :)")
      }

    case _ =>
  }
}