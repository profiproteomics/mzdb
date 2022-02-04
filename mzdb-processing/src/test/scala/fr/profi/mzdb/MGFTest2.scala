package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.io.writer.mgf.IsolationWindowPrecursorExtractor2
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

@Ignore
class MGFTest2 extends StrictLogging {

  private val CONCORDANT = "concordant"
  private val UP_QX = "up QX"
  private val UP_MZDB = "up mzdb"

  private val metric = new Metric("MGFTest")
  private val annotations = Array("scan.number", "ident.status", "ident.moz", "ident.charge", "found", "rank" , "intensity", "rank0.intensity", "rank0.moz", "intensity.ratio",
    "prediction.moz", "prediction.charge", "prediction.note", "prediction.intensity", "prediction.rank", "rank0.intensity", "rank0.prediction.moz", "rank0.prediction.charge", "rank0.prediction.note", "rank0.prediction.rank",
    "cause", "header.moz", "header.charge", "sw_center.moz")

  case class Identification(scan: Int, moz:Double, charge:Int, initMoz:Double, initCharge:Int, status:String)

  def dumpStats(map : Map[String, Any], fw : BufferedWriter) = {

      val values = annotations.map{ k => map.getOrElse(k, "") }
      fw.write(values.mkString("\t"))
      fw.newLine()
  }


  @Test
  def testMGFGeneration() = {

    val mzTol = 10.0f

    val lines = Source.fromInputStream(MGFTest2.this.getClass.getResourceAsStream("/run_2790.csv")).getLines().drop(1).toSeq
    val idents = for {line <- lines
                      values = line.split(";").map(_.trim)}
    yield Identification(values(0).split("_")(1).toInt, values(1).toDouble, values(2).toInt, values(3).toDouble, values(4).toInt, values(5))

    logger.info("nb identifications = {}", idents.length)
    val identsByScan = idents.groupBy(_.scan).map { case (k, v) => k -> v.head }

    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"

    val fw = new BufferedWriter(new FileWriter(new File((new File(mzdbFilePath)).getParentFile, "precursors_stats_full.txt" )))
    fw.write(annotations.mkString("\t"))
    fw.newLine()

    val precComputer = new IsolationWindowPrecursorExtractor2(mzTol)

    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    mzDbReader.enablePrecursorListLoading()
    mzDbReader.enableScanListLoading()

    for( (scan, identification) <- identsByScan  ) {

      val spectrumHeader = mzDbReader.getSpectrumHeader(scan)
      var map = precComputer.extractPrecursorStats(mzDbReader, spectrumHeader, identification.moz, mzTol)
      map += ("scan.number" -> scan)
      map += ("ident.status" -> identification.status)
      map += ("ident.moz" -> identification.moz)
      map += ("ident.charge" -> identification.charge)

      dumpStats(map, fw)
    }


    logger.info(metric.toString());
    precComputer.dumpMetrics()
    fw.flush
    fw.close
    ()
  }


}
