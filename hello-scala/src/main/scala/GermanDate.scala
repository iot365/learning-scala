import java.text.DateFormat._
import java.util.{Date, Locale}

object GermanDate {
  def main(args: Array[String]) {
    val now = new Date
    val german: Locale = Locale.GERMAN
    println(getDateInstance(FULL, german) format now)
    println(getDateInstance(LONG, german) format now)
    println(getDateInstance(MEDIUM, german) format now)
    println(getDateInstance(SHORT, german) format now)
  }
}