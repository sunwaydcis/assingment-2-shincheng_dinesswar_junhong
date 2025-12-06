import com.github.tototoshi.csv.* 
import java.io.InputStreamReader 
import java.time.LocalDate 
import java.time.format.DateTimeFormatter 
import scala.util.{Try, Using} 
 
// CSV Loader — Loads CSV data from resources folder
object HotelCSVLoader: 
  private val UTF8 = "UTF-8" 
 
  def loadFromResources(filename: String): List[Map[String, String]] = 
    // Find file inside src/main/resources
    Option(getClass.getResourceAsStream(s"/$filename")) match 
      case None => 
        // File not found
        println(s"❌ Failed to load dataset: $filename (File not found in resources)") 
        Nil 

      case Some(stream) => 
        // Read CSV with header → returns List[Map]
        Using.resource(CSVReader.open(new InputStreamReader(stream, UTF8))) { reader => 
          reader.allWithHeaders().toList 
        } 
  end loadFromResources 
end HotelCSVLoader 
 
// Data types — Structure for storing hotel info cleanly
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

  // Safe map access with default value to avoid crashes
  def safeGet(map: Map[String, String], key: String, default: String = "0"): String = 
    map.getOrElse(key, default).trim 

  // Convert string to Double, clean symbols such as %, [SGD]
  def toDouble(s: String): Double = 
    val clean = s.replaceAll("[\\[\\]%]", "").trim 
    Try(clean.toDouble).getOrElse(0.0) match { 
      // If number > 1 but contains %, convert back to decimal form
      case x if x > 1 && s.contains("%") => x / 100.0 
      case x => x 
    } 

  // Safe string → Int
  def toInt(s: String): Int = 
    Try(s.trim.toInt).getOrElse(0) 

  // Calculate number of nights between check-in and check-out date
  def nightsBetween(checkIn: String, checkOut: String): Int = 
    Try { 
      val start = LocalDate.parse(checkIn, DateFormatter) 
      val end   = LocalDate.parse(checkOut, DateFormatter) 
      math.max(1, java.time.temporal.ChronoUnit.DAYS.between(start, end).toInt) 
    }.getOrElse(1) 

  // Return (min, max) pair
  def minMax(values: List[Double]): (Double, Double) = 
    if values.isEmpty then (0.0, 0.0) 
    else (values.min, values.max) 

  // Normalize values to 0–1 range
  def normalize(value: Double, min: Double, max: Double): Double = 
    if (max - min).abs < 1e-10 then 0.0 
    else (value - min) / (max - min) 

  // Average helper
  def mean(values: List[Double]): Double = 
    if values.isEmpty then 0.0 
    else values.sum / values.length 
end DataUtils 

// Analysis Trait 
trait Analyzer: 
  def analyze(data: List[Map[String, String]]): Unit 

  // Pretty header formatting
  protected def printHeader(title: String): Unit =
    println(s"\n$title")
    println("─" * title.length)
 
// Q1. Country with The Highest Number of Bookings 
class CountryWithMostBookings extends Analyzer: 
  override def analyze(data: List[Map[String, String]]): Unit = 
    printHeader("🌍 QUESTION 1 — Country With Highest Number of Bookings") 

    // Group all rows by Country → count rows per country
    val countryCounts = data 
      .groupBy(_("Destination Country")) 
      .view 
      .mapValues(_.size) 

    // Find country with largest booking count
    countryCounts.maxByOption(_._2) match 
      case Some((country, count)) => 
        println(s" ✅ Country: $country") 
        println(s"    Total Bookings: $count") 
      case None => 
        println(" ❌ No data available.") 
    println("─" * 56)

// 2. Most Economical Hotel based on Booking Price per night & Discount & Profit Margin
class MostEconomicalHotel extends Analyzer:
  import DataUtils._

  // Compute hotel metrics for each unique hotel
  private def computeHotelMetrics(data: List[Map[String, String]]): List[HotelMetrics] =
    data
      .groupBy: row =>
        // Key = country + city + hotel name
        HotelIdentifier(
          safeGet(row, "Destination Country"),
          safeGet(row, "Destination City"),
          safeGet(row, "Hotel Name")
        )
      .toList
      .flatMap: (id, rows) =>
        // Compute price per night for each booking row
        val costPerNightList = rows.map: row =>
          val price  = toDouble(safeGet(row, "Booking Price[SGD]"))
          val rooms  = math.max(1, toInt(safeGet(row, "Rooms")))
          val nights = nightsBetween(
            safeGet(row, "Check-in date"),
            safeGet(row, "Check-Out Date")
          )
          price / rooms / nights

// Skip invalid entries
if costPerNightList.isEmpty then None
        else
          Some(HotelMetrics(
            identifier = id,
            avgCostPerNight = mean(costPerNightList),
            avgDiscount = mean(rows.map(r => toDouble(safeGet(r, "Discount")))),
            avgProfitMargin = mean(rows.map(r => toDouble(safeGet(r, "Profit Margin")))),
            totalVisitors = rows.map(r => toInt(safeGet(r, "No. Of People"))).sum
          ))

// Normalize scores and compute final ranking score
private def calculateScores(metrics: List[HotelMetrics]): List[(HotelMetrics, NormalizedScores, Double)] =
    val costs = metrics.map(_.avgCostPerNight)
    val discounts = metrics.map(_.avgDiscount)
    val profits = metrics.map(_.avgProfitMargin)

    val (minCost, maxCost) = minMax(costs)
    val (minDisc, maxDisc) = minMax(discounts)
    val (minProf, maxProf) = minMax(profits)

metrics.map: metric =>
      // Lower cost → higher score
      val priceScore = 1 - normalize(metric.avgCostPerNight, minCost, maxCost)

      // Higher discount → better score
      val discountScore = normalize(metric.avgDiscount, minDisc, maxDisc)

      // Lower profit margin (cheaper for customer) → higher score
      val profitScore = 1 - normalize(metric.avgProfitMargin, minProf, maxProf)

      // Equal weighting for 3 factors
      val finalScore = (priceScore + discountScore + profitScore) / 3.0

      (metric, NormalizedScores(priceScore, discountScore, profitScore), finalScore)

override def analyze(data: List[Map[String, String]]): Unit =
    printHeader("🏨 QUESTION 2 — Most Economical Hotel (Price, Discount, Profit Margin)")

    computeHotelMetrics(data) match
      case Nil =>
        println(" ❌ No hotel entries found.")
        println("─" * 56)

      case metrics =>
        // Score all hotels
        val scoredHotels = calculateScores(metrics)

        // Find top ranked hotel
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

// 3. Most Profitable Hotel based on Total visitors & Average profit margin
class MostProfitableHotel extends Analyzer: 
  import DataUtils._ 
 
  override def analyze(data: List[Map[String, String]]): Unit = 
    printHeader("💰 QUESTION 3 — Most Profitable Hotel (Visitors + Profit Margin)")

// Convert rows into Booking-like structure 
    val bookings = data.flatMap: row => 
      Try { 
        val hotel   = safeGet(row, "Hotel Name") 
        val country = safeGet(row, "Destination Country") 
        val city    = safeGet(row, "Destination City") 
        val visitors = toInt(safeGet(row, "No. Of People")) 
        val margin   = toDouble(safeGet(row, "Profit Margin")) 
        (hotel, country, city, visitors.toDouble, margin) 
      }.toOption 
 
    if bookings.isEmpty then 
      println(" ❌ No valid booking records for profitability evaluation.") 
      return 
 
    // Group profitability by unique hotel 
    val results = bookings.groupBy(b => (b._1, b._2, b._3)).map { case ((hotel, country, city), group) => 
 
          val visitorList = group.map(_._4) 
          val marginList  = group.map(_._5) 
     
          // Get ranges for normalization
          val (minVisitors, maxVisitors) = DataUtils.minMax(visitorList) 
          val (minMargin, maxMargin)     = DataUtils.minMax(marginList) 
 
          val totalVisitors = visitorList.sum 
          val avgMargin     = marginList.sum / marginList.length
     
          // Normalization of visitor score (higher = better)
          val visitorScore = 
            if (maxVisitors - minVisitors == 0) 1.0 
            else (totalVisitors - minVisitors) / (maxVisitors - minVisitors) 

          // Normalization of profit margin score (higher = better)
          val marginScore = 
            if (maxMargin - minMargin == 0) 1.0 
            else (avgMargin - minMargin) / (maxMargin - minMargin) 

          // Final combined profitability score
          val finalScore = (visitorScore + marginScore) / 2 
 
          (hotel, country, city, totalVisitors, avgMargin, visitorScore, marginScore, finalScore) 
        } 
        .toList 
        .sortBy(-_._8) 
 
    // Print top 10 hotels
    results.take(10).foreach { case (hotel, country, city, totVisitors, avgMargin, vScore, mScore, score) => 
      println(f"$hotel%-25s | $country%-15s | $city%-15s | Visitors: $totVisitors%5.0f | Avg Profit: ${avgMargin}%.4f | Score: ${score}%.4f") 
    } 
 
    // Print the best hotel 
    println() 
    println(s"🔥 MOST PROFITABLE HOTEL OVERALL: ${results.head._1}") 
    println("─" * 56) 

// Main Application 
object HotelAnalysisApp: 
  def main(args: Array[String]): Unit = 
    println("=" * 60) 
    println("          HOTEL BOOKING ANALYSIS SYSTEM") 
    println("=" * 60) 
 
    // Load data 
    val data = HotelCSVLoader.loadFromResources("Hotel_Dataset.csv") 
    if data.isEmpty then 
      println("❌ No data loaded. Exiting.") 
      return 
 
    println(s"✅ Loaded ${data.size} booking records") 
    println() 
 
    // Run analyses 
    val analyzers: List[Analyzer] = List( 
      new CountryWithMostBookings, 
      new MostEconomicalHotel, 
      new MostProfitableHotel 
    ) 

    // Execute each question sequentially
    analyzers.foreach(_.analyze(data)) 
 
    println("\n" + "=" * 60) 
    println("          ANALYSIS COMPLETE") 
    println("=" * 60) 
end HotelAnalysisApp 

