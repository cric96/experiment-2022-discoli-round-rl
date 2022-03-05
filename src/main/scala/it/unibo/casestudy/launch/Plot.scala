package it.unibo.casestudy.launch

import com.github.tototoshi.csv.CSVReader
import it.unibo.casestudy.launch.LaunchConstant._
import org.nspl._
import org.nspl.awtrenderer._

import java.io.File

object Plot extends App {
  // Prepare
  type ExperimentData = (Double, Double, Double)
  val toSecondConversion = 1000.0
  val resultFolder = os.pwd / LaunchConstant.resFolder
  val imageFolder = os.pwd / LaunchConstant.imageFolder
  if (os.exists(imageFolder)) { os.remove.all(imageFolder) }
  os.makeDir.all(imageFolder)
  val allFiles = os.list(resultFolder).filter(os.isFile).filter(_.toString.contains(".csv"))

  // Load data
  val (_, fixed) = load(allFiles, fixedName, convertFromString).head
  val (_, adHoc) = load(allFiles, adhocName, convertFromString).head
  // One folder for each configuration
  allExperiment(resultFolder).foreach { rlFolder =>
    val allFiles = os.list(rlFolder).filter(os.isFile).filter(_.toString.contains(".csv"))
    // One file foreach episode
    val rl = load(allFiles, rlName, convertFromString)
    val (_, error) = load(allFiles, errorName, convertSingle).head
    val (_, totalTicks) = load(allFiles, totalTicksName, convertSingle).head
    // Plots preparation
    val errorPlot = xyplot(
      (error, List(line(color = Color.red)), InLegend("Error"))
    )(
      par(xlab = "time", ylab = "Root Mean Squared Error")
    )
    val totalTickPlot = xyplot(
      (totalTicks, List(line(color = Color.apply(255, 255, 0))), InLegend("Total ticks per second"))
    )(
      par(xlab = "time", ylab = "Ticks per seconds")
    )
    os.makeDir(imageFolder / rlFolder.baseName)
    // Plot storage
    rl.foreach { case (name, data) =>
      scribe.info(s"process: $name")
      plotRl(imageFolder / rlFolder.baseName, data, fixed, adHoc, name)
    }
    store(renderToFile(errorPlot), imageFolder / rlFolder.baseName / s"error.png")
    store(renderToFile(totalTickPlot), imageFolder / rlFolder.baseName / s"ticks.png")
  }

  // Utility functions
  def convertFromString(data: List[String]): ExperimentData =
    (data.head.toLong / toSecondConversion, data(1).toDouble, data(2).toDouble)

  def convertSingle(data: List[String]): Double = data.head.toDouble

  def allExperiment(resultFolder: os.Path): Seq[os.Path] = os.list(resultFolder).filter(os.isDir)
  def load[E](allFiles: Seq[os.Path], target: String, conversion: List[String] => E): (Seq[(String, Seq[E])]) =
    allFiles
      .filter(_.baseName.contains(target))
      .map(f => f.baseName -> CSVReader.open(f.toIO))
      .map { case (name, reader) => (name, reader.all().tail, reader) }
      .tapEach(_._3.close())
      .map { case (name, dataL, _) => name -> dataL.map(conversion) }

  def plotRl(
      where: os.Path,
      rl: Seq[ExperimentData],
      fixed: Seq[ExperimentData],
      adhoc: Seq[ExperimentData],
      label: String = ""
  ): Unit = {
    def convert(data: Seq[ExperimentData], select: (ExperimentData) => Double) = data.map { case t @ (time, _, _) =>
      (time, select(t))
    }

    def tickPerSeconds(trace: Seq[ExperimentData]): Seq[(Double, Double)] = {
      trace.dropRight(1).zip(trace.tail).map { case (first, second) =>
        (first._1, second._2 - first._2)
      }
    }

    val outputPlot = xyplot(
      (convert(fixed, _._3), List(line(color = Color.red)), InLegend("Periodic")),
      (convert(adhoc, _._3), List(line(color = Color.green)), InLegend("Ad Hoc")),
      (convert(rl, _._3), List(line(color = Color.blue)), InLegend("Rl"))
    )(
      par(xlab = "time", ylab = "total output")
    )

    val totalTicksPlot = xyplot(
      (convert(fixed, _._2), List(line(color = Color.red)), InLegend("Periodic")),
      (convert(adhoc, _._2), List(line(color = Color.green)), InLegend("Ad Hoc")),
      (convert(rl, _._2), List(line(color = Color.blue)), InLegend("Rl"))
    )(
      par(xlab = "time", ylab = "total ticks")
    )

    val frequencyPlot = xyplot(
      (tickPerSeconds(fixed), List(line(color = Color.red)), InLegend("Periodic")),
      (tickPerSeconds(adhoc), List(line(color = Color.green)), InLegend("Ad Hoc")),
      (tickPerSeconds(rl), List(line(color = Color.blue)), InLegend("Rl"))
    )(
      par(xlab = "time", ylab = "ticks per second")
    )

    store(
      renderToFile(sequence(List(outputPlot, totalTicksPlot, frequencyPlot), TableLayout(2))),
      where / s"image-$label.png"
    )
  }

  def store(file: File, path: os.Path): Unit =
    os.copy.over(os.Path(file.toPath), path)
}
