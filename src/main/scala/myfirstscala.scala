import scala.io.Source
import scala.util.Try

object HotelBookingAnalyzer {

  def main(args: Array[String]): Unit = {
    println("=" * 60)
    println("HOTEL BOOKING DATA ANALYSIS")
    println("QUESTION 1: Which country has the highest number of bookings?")
    println("=" * 60)

    // Try to read and analyze the data
    analyzeBookings()
  }

  def analyzeBookings(): Unit = {
    val filename = "hotel_bookings.csv"  // Make sure this matches your CSV filename

    println(s"\nLooking for data file: $filename")

    // Check if file exists
    val file = new java.io.File(filename)
    if (!file.exists()) {
      println(s"❌ ERROR: File '$filename' not found!")
      println("Please make sure 'hotel_bookings.csv' is in the same folder as:")
      println(s"  ${new java.io.File(".").getAbsolutePath}")
      return
    }

    println("✅ File found! Reading data...")

    try {
      // Read the entire file
      val source = Source.fromFile(filename)
      val allLines = source.getLines().toList
      source.close()

      if (allLines.isEmpty) {
        println("❌ ERROR: CSV file is empty!")
        return
      }

      println(s"📊 Total lines in file: ${allLines.size}")

      // Extract header
      val header = allLines.head
      println(s"📋 File header: $header")
      println()

      // Get all data lines (skip header)
      val dataLines = allLines.tail
      println(s"📝 Processing ${dataLines.size} booking records...")
      println()

      // Extract Origin Countries from each line (2nd column, index 1)
      val originCountries = dataLines.flatMap { line =>
        val parts = line.split(",").map(_.trim)
        if (parts.length >= 3) {
          // Extract Origin_Country (2nd column)
          val country = parts(1)  // Origin_Country is at index 1
          Some(country)
        } else {
          println(s"⚠️  Warning: Skipping invalid line: $line")
          None
        }
      }

      // Extract Destination Countries from each line (3rd column, index 2)
      val destinationCountries = dataLines.flatMap { line =>
        val parts = line.split(",").map(_.trim)
        if (parts.length >= 3) {
          // Extract Destination_Country (3rd column)
          val country = parts(2)  // Destination_Country is at index 2
          Some(country)
        } else {
          None
        }
      }

      if (originCountries.isEmpty && destinationCountries.isEmpty) {
        println("❌ ERROR: No valid country data found in the file!")
        println("Check that your CSV file has at least 3 columns.")
        return
      }

      // ANALYSIS 1: Origin Countries
      println("=" * 60)
      println("ANALYSIS 1: ORIGIN COUNTRIES (Where travelers come from)")
      println("=" * 60)

      val originCountryCounts = originCountries
        .groupBy(country => country)
        .map { case (country, list) => (country, list.size) }
        .toList

      if (originCountryCounts.nonEmpty) {
        val (topOriginCountry, topOriginCount) = originCountryCounts.maxBy { case (country, count) => count }

        println("\n✅ ORIGIN COUNTRY WITH HIGHEST NUMBER OF BOOKINGS:")
        println(s"   🏆 $topOriginCountry")
        println(s"   📅 $topOriginCount bookings")

        println("\n📊 ALL ORIGIN COUNTRIES (Ranked by Bookings):")
        println("-" * 40)

        val sortedOriginCountries = originCountryCounts.sortBy { case (country, count) => -count }
        sortedOriginCountries.zipWithIndex.foreach { case ((country, count), index) =>
          val rank = index + 1
          val stars = if (rank == 1) "⭐ " else s"$rank. "
          println(f"$stars%-4s $country%-15s $count%5d bookings")
        }
      }

      // ANALYSIS 2: Destination Countries
      println("\n" + "=" * 60)
      println("ANALYSIS 2: DESTINATION COUNTRIES (Where travelers go to)")
      println("=" * 60)

      val destinationCountryCounts = destinationCountries
        .groupBy(country => country)
        .map { case (country, list) => (country, list.size) }
        .toList

      if (destinationCountryCounts.nonEmpty) {
        val (topDestCountry, topDestCount) = destinationCountryCounts.maxBy { case (country, count) => count }

        println("\n✅ DESTINATION COUNTRY WITH HIGHEST NUMBER OF BOOKINGS:")
        println(s"   🏆 $topDestCountry")
        println(s"   📅 $topDestCount bookings")

        println("\n📊 ALL DESTINATION COUNTRIES (Ranked by Bookings):")
        println("-" * 40)

        val sortedDestCountries = destinationCountryCounts.sortBy { case (country, count) => -count }
        sortedDestCountries.zipWithIndex.foreach { case ((country, count), index) =>
          val rank = index + 1
          val stars = if (rank == 1) "⭐ " else s"$rank. "
          println(f"$stars%-4s $country%-15s $count%5d bookings")
        }
      }

      // ANALYSIS 3: Hotel Popularity
      println("\n" + "=" * 60)
      println("BONUS ANALYSIS: MOST POPULAR HOTELS")
      println("=" * 60)

      val hotels = dataLines.flatMap { line =>
        val parts = line.split(",").map(_.trim)
        if (parts.length >= 4) {
          Some(parts(3))  // Hotel_Name is at index 3
        } else {
          None
        }
      }

      val hotelCounts = hotels
        .groupBy(hotel => hotel)
        .map { case (hotel, list) => (hotel, list.size) }
        .toList

      if (hotelCounts.nonEmpty) {
        val (topHotel, topHotelCount) = hotelCounts.maxBy { case (hotel, count) => count }

        println(s"\n🏨 MOST POPULAR HOTEL: $topHotel ($topHotelCount bookings)")

        println("\n📊 ALL HOTELS (Ranked by Bookings):")
        println("-" * 40)

        val sortedHotels = hotelCounts.sortBy { case (hotel, count) => -count }
        sortedHotels.zipWithIndex.foreach { case ((hotel, count), index) =>
          val rank = index + 1
          val stars = if (rank == 1) "⭐ " else s"$rank. "
          println(f"$stars%-4s $hotel%-20s $count%5d bookings")
        }
      }

      println()
      println("=" * 60)
      println("✅ All analyses completed successfully!")
      println("=" * 60)

    } catch {
      case e: Exception =>
        println(s"❌ UNEXPECTED ERROR: ${e.getMessage}")
        println("Stack trace for debugging:")
        e.printStackTrace()
    }
  }
}

// Test with your data
object TestWithYourData {
  def main(args: Array[String]): Unit = {
    println("Testing with your CSV data...")
    println("=" * 60)

    // Your actual data
    val yourData = List(
      "B001,Indonesia,New Zealand,Grand Hyatt,500.0,50.0,100.0",
      "B002,Malaysia,New Zealand,Grand Hyatt,450.0,45.0,90.0",
      "B003,Singapore,New Zealand,Marriott,550.0,55.0,110.0",
      "B004,Thailand,Nepal,Hilton,400.0,40.0,80.0",
      "B005,Indonesia,New Zealand,Hyatt,480.0,48.0,96.0",
      "B006,Malaysia,Iran,Sheraton,350.0,35.0,70.0",
      "B007,Singapore,New Zealand,Grand Hyatt,520.0,52.0,104.0",
      "B008,Thailand,Nepal,Marriott,420.0,42.0,84.0",
      "B009,Indonesia,New Zealand,Hilton,460.0,46.0,92.0",
      "B010,Malaysia,Iran,Hyatt,380.0,38.0,76.0"
    )

    // Test origin countries
    val originCountries = yourData.map(_.split(",")(1))
    val originCounts = originCountries.groupBy(x => x).mapValues(_.size)
    println("Origin Country Counts:")
    originCounts.toList.sortBy(-_._2).foreach { case (country, count) =>
      println(s"  $country: $count bookings")
    }

    // Test destination countries
    val destCountries = yourData.map(_.split(",")(2))
    val destCounts = destCountries.groupBy(x => x).mapValues(_.size)
    println("\nDestination Country Counts:")
    destCounts.toList.sortBy(-_._2).foreach { case (country, count) =>
      println(s"  $country: $count bookings")
    }

    println("\nExpected Output:")
    println("Top Origin Country: Indonesia (3 bookings)")
    println("Top Destination Country: New Zealand (6 bookings)")
  }
}
