import com.github.tototoshi.csv._

import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter


object Main extends App {
  private val HydroCSV = CSVReader.open(new File("./src/main/scala/Hydro.csv"))
  private val SolarCSV = CSVReader.open(new File("./src/main/scala/Solar.csv"))
  private val WindCSV = CSVReader.open(new File("./src/main/scala/Wind.csv"))
  private val HydroData = HydroCSV.all()
  private val SolarData = SolarCSV.all()
  private val WindData = WindCSV.all()
  
  case class RenewableData(startTime: LocalDateTime, endTime: LocalDateTime, hydroPowerProduction: Double)
  
  val dateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
  
  val noHeaderValuesHydro = HydroData.drop(1) //remove header
  val dataHydro = noHeaderValuesHydro.map { line =>
    RenewableData(LocalDateTime.parse(line(0), dateTimeFormatter), LocalDateTime.parse(line(1), dateTimeFormatter), line(2).toDouble)
  }
  
  val noHeaderValuesSolar = SolarData.drop(1) //remove header
  val dataSolar = noHeaderValuesSolar.map { line =>
    RenewableData(LocalDateTime.parse(line(0), dateTimeFormatter), LocalDateTime.parse(line(1), dateTimeFormatter), line(2).toDouble)
  }
  
  val noHeaderValuesWind = WindData.drop(1) //remove header
  val dataWind = noHeaderValuesWind.map { line =>
    RenewableData(LocalDateTime.parse(line(0), dateTimeFormatter), LocalDateTime.parse(line(1), dateTimeFormatter), line(2).toDouble)
  }
  
  def filterDataByTimePeriod(data: Seq[RenewableData], startTime: LocalDateTime, endTime: LocalDateTime): Seq[RenewableData] = {
    data.filter(d => d.startTime.isAfter(startTime) && d.endTime.isBefore(endTime))
  }
  
  def viewData() = {
    print("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val choice = scala.io.StdIn.readInt()
    print("Filter By:\n 1. Last hour\n 2. Last day\n 3. Last week\n 4. Last month\nPlease enter your choice: ")
    val choice2 = scala.io.StdIn.readInt()
    
    val startTime = choice2 match {
      case 1 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        currentTime.minusHours(1)
      case 2 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        currentTime.minusDays(1)
      case 3 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        currentTime.minusDays(7)
      case 4 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        currentTime.minusDays(30)
    }
    
    choice match {
      case 1 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        val endTime = currentTime
        val filteredData = filterDataByTimePeriod(dataHydro, startTime, endTime)
        
        filteredData.foreach(println)
      case 2 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        val endTime = currentTime
        val filteredData = filterDataByTimePeriod(dataSolar, startTime, endTime)
        
        filteredData.foreach(println)
      case 3 =>
        val currentTime = LocalDateTime.now() // Get the current date-time
        
        val endTime = currentTime
        val filteredData = filterDataByTimePeriod(dataWind, startTime, endTime)
        
        filteredData.foreach(println)
      case _ =>
        println("Invalid choice. Please try again.")
    }
  }
  
  
  def mean(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val mean = numericValues.sum / numericValues.length
      println(s"Mean: $mean")
    } else {
      println("No numeric data available.")
    }
  }

  def median(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption).sorted

    if (numericValues.nonEmpty) {
      val length = numericValues.length
      val median =
        if (length % 2 == 0)
          (numericValues(length / 2 - 1) + numericValues(length / 2)) / 2.0
        else
          numericValues(length / 2)

      println(s"Median: $median")
    } else {
      println("No numeric data available.")
    }
  }

  def mode(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val occurrences = values.groupBy(identity).mapValues(_.size)

    if (occurrences.nonEmpty) {
      val maxCount = occurrences.values.max
      val modes = occurrences.filter(_._2 == maxCount).keys.toList

      if (modes.length == 1) {
        println(s"Mode: ${modes.head}")
      } else {
        println("Multiple modes found. Choosing one mode:")
        println(s"Mode: ${modes.head}")
      }
    } else {
      println("No data available.")
    }
  }

  def range(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val minValue = numericValues.min
      val maxValue = numericValues.max
      val rangeValue = maxValue - minValue

      println(s"Range: $rangeValue")
    } else {
      println("No numeric data available.")
    }
  }

  def midrange(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val minValue = numericValues.min
      val maxValue = numericValues.max
      val midrangeValue = (minValue + maxValue) / 2

      println(s"Midrange: $midrangeValue")
    } else {
      println("No numeric data available.")
    }
  }

  private var running = true

  while (running) {
    println("Menu:")
    println("1. View Hydro Data")
    println("2. View Solar Data")
    println("3. View Data")
    println("4. Exit")

    print("Please enter your choice: ")
    val choice = scala.io.StdIn.readInt()

    choice match {
      case 1 =>
        mean(HydroData)
        median(HydroData)
        mode(HydroData)
        range(HydroData)
        midrange(HydroData)
      case 2 =>
        mean(SolarData)
        median(SolarData)
        mode(SolarData)
        range(SolarData)
        midrange(SolarData)
      case 3 =>
        viewData()
      case 4 =>
        running = false
        println("Goodbye!")
      case _ =>
        println("Invalid choice. Please try again.")
    }
    println()
  }
}
