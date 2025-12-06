import com.github.tototoshi.csv.* 
import java.io.InputStreamReader 
import java.time.LocalDate 
import java.time.format.DateTimeFormatter 
import scala.util.{Try, Using} 
 
// CSV Loader 
object HotelCSVLoader: 
  private val UTF8 = "UTF-8" 
 
  def loadFromResources(filename: String): List[Map[String, String]] = 
    Option(getClass.getResourceAsStream(s"/$filename")) match 
      case None => 
        println(s"❌ Failed to load dataset: $filename (File not found in resources)") 
        Nil 
      case Some(stream) => 
        Using.resource(CSVReader.open(new InputStreamReader(stream, UTF8))) { reader => 
          reader.allWithHeaders().toList 
        } 
  end loadFromResources 
end HotelCSVLoader 
 
// Data types 
case class HotelIdentifier(country: String, city: String, name: String) 
case class HotelMetrics( 
                         identifier: HotelIdentifier, 
                         avgCostPerNight: Double, 
                         avgDiscount: Double, 
                         avgProfitMargin: Double, 
                         totalVisitors: Int 
                       ) 
case class NormalizedScores(price: Double, discount: Double, profit: Double) 
 
// Utils — Helping for parsing and calculations 
object DataUtils: 
  private val DateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy") 
 
  def safeGet(map: Map[String, String], key: String, default: String = "0"): String = 
    map.getOrElse(key, default).trim 
 
  def toDouble(s: String): Double = 
    Try(s.replaceAll("[\\[\\]%]", "").trim.toDouble).getOrElse(0.0) 
 
  def toInt(s: String): Int = 
    Try(s.trim.toInt).getOrElse(0) 
 
  def nightsBetween(checkIn: String, checkOut: String): Int = 
    Try { 
      val start = LocalDate.parse(checkIn, DateFormatter) 
      val end   = LocalDate.parse(checkOut, DateFormatter) 
      math.max(1, java.time.temporal.ChronoUnit.DAYS.between(start, end).toInt) 
    }.getOrElse(1) 
 
  def minMax(values: List[Double]): (Double, Double) = 
    if values.isEmpty then (0.0, 0.0) 
    else (values.min, values.max) 
 
  def normalize(value: Double, min: Double, max: Double): Double = 
    if (max - min).abs < 1e-10 then 0.0 
    else (value - min) / (max - min) 
 
  def mean(values: List[Double]): Double = 
    if values.isEmpty then 0.0 
    else values.sum / values.length 
end DataUtils 
 
// Analysis Trait 
trait Analyzer: 
  def analyze(data: List[Map[String, String]]): Unit 
  protected def printHeader(title: String): Unit = 
    println(s"\n$title") 
    println("─" * title.length) 
 
// 1. Country with Most Bookings 
class CountryWithMostBookings extends Analyzer: 
  override def analyze(data: List[Map[String, String]]): Unit = 
    printHeader("🌍 QUESTION 1 — Country With Highest Number of Bookings") 
 
    val countryCounts = data 
      .groupBy(_("Destination Country")) 
      .view 
      .mapValues(_.size) 
 
    countryCounts.maxByOption(_._2) match 
      case Some((country, count)) => 
        println(s" ✅ Country: $country") 
        println(s"    Total Bookings: $count") 
      case None => 
        println(" ❌ No data available.") 
    println("─" * 56)

// 2. Most Economical Hotel
class MostEconomicalHotel extends Analyzer:
  import DataUtils._

  private def computeHotelMetrics(data: List[Map[String, String]]): List[HotelMetrics] =
    data
      .groupBy: row =>
        HotelIdentifier(
          safeGet(row, "Destination Country"),
          safeGet(row, "Destination City"),
          safeGet(row, "Hotel Name")
        )
      .toList
      .flatMap: (id, rows) =>
        val costPerNightList = rows.map: row =>
          val price  = toDouble(safeGet(row, "Booking Price[SGD]"))
          val rooms  = math.max(1, toInt(safeGet(row, "Rooms")))
          val nights = nightsBetween(
            safeGet(row, "Check-in date"),
            safeGet(row, "Check-Out Date")
          )
          price / rooms / nights

if costPerNightList.isEmpty then None
        else
          Some(HotelMetrics(
            identifier = id,
            avgCostPerNight = mean(costPerNightList),
            avgDiscount = mean(rows.map(r => toDouble(safeGet(r, "Discount")))),
            avgProfitMargin = mean(rows.map(r => toDouble(safeGet(r, "Profit Margin")))),
            totalVisitors = rows.map(r => toInt(safeGet(r, "No. Of People"))).sum
          ))

