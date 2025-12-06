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

private def calculateScores(metrics: List[HotelMetrics]): List[(HotelMetrics, NormalizedScores, Double)] =
    val costs = metrics.map(_.avgCostPerNight)
    val discounts = metrics.map(_.avgDiscount)
    val profits = metrics.map(_.avgProfitMargin)

    val (minCost, maxCost) = minMax(costs)
    val (minDisc, maxDisc) = minMax(discounts)
    val (minProf, maxProf) = minMax(profits)

metrics.map: metric =>
      val priceScore = 1 - normalize(metric.avgCostPerNight, minCost, maxCost)
      val discountScore = normalize(metric.avgDiscount, minDisc, maxDisc)
      val profitScore = 1 - normalize(metric.avgProfitMargin, minProf, maxProf)
      val finalScore = (priceScore + discountScore + profitScore) / 3.0

      (metric, NormalizedScores(priceScore, discountScore, profitScore), finalScore)

override def analyze(data: List[Map[String, String]]): Unit =
    printHeader("🏨 QUESTION 2 — Most Economical Hotel (Price, Discount, Profit Margin)")

    computeHotelMetrics(data) match
      case Nil =>
        println(" ❌ No hotel entries found.")
        println("─" * 56)

      case metrics =>
        val scoredHotels = calculateScores(metrics)
        val bestHotel = scoredHotels.maxByOption(_._3)

        bestHotel match
          case Some((metric, scores, finalScore)) =>
            val id = metric.identifier
            println(s" ✅ Hotel: ${id.name} (${id.city}, ${id.country})")
            println(f"    Price Score      : ${scores.price}%.4f")
            println(f"    Discount Score   : ${scores.discount}%.4f")
            println(f"    Profit Score     : ${scores.profit}%.4f")
            println(f"    FINAL ECON SCORE : $finalScore%.4f")
          case None =>
            println(" ❌ No hotel could be scored.")
        println("─" * 56)

// 3. Most Profitable Hotel
