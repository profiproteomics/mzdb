package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.io.writer.mgf.{IsolationWindowPrecursorExtractor_v3_6, IsolationWindowPrecursorExtractor_v3_7}
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

@Ignore
/**
 *   Search the moz of the identified precursors in the mzdb file by targeting the scan number and extract some metrics about
 *   signal found in the raw data. The extracted stats are dumped into a txt file.
 */
class SearchIdentifiedPrecursorsTest extends StrictLogging {

  private val metric = new Metric("SearchIdentifiedPrecursorsTest")
  private val annotations = Array("scan.number", "scan.rt", "ident.status", "ident.moz", "ident.charge", "ident.score_max", "ident.score_from", "found",
    "ident.initial.rank", "ident.initial.moz" , "ident.initial.intensity", "ident.prediction.moz", "ident.prediction.charge", "ident.prediction.note", "ident.prediction.intensity", "ident.prediction.rank",
    "rank0.initial.intensity", "rank0.initial.moz", "rank0.prediction.moz", "rank0.prediction.charge", "rank0.prediction.note", "rank0.prediction.rank",
    "swcenter.initial.rank", "swcenter.initial.moz", "swcenter.initial.intensity", "swcenter.prediction.moz", "swcenter.prediction.charge", "swcenter.prediction.note", "swcenter.prediction.rank",
    "cause", "header.moz", "header.charge", "header.found", "sw_center.moz")

  case class Identification(scan: Int, moz:Double, charge:Int, initMoz:Double, initCharge:Int, status:String, scoreMax: Double, scoreFrom: String)

  def dumpStats(map : Map[String, Any], fw : BufferedWriter) = {

      val values = annotations.map{ k => map.getOrElse(k, "") }
      fw.write(values.mkString("\t"))
      fw.newLine()
  }


  @Test
  def testMGFGeneration() = {

    val mzTol = 10.0f

    val lines = Source.fromInputStream(SearchIdentifiedPrecursorsTest.this.getClass.getResourceAsStream("/run_2790.csv")).getLines().drop(1).toSeq
    val idents = for {line <- lines
                      values = line.split(";").map(_.trim)}
    yield Identification(values(0).split("_")(1).toInt, values(1).toDouble, values(2).toInt, values(3).toDouble, values(4).toInt, values(5), values(6).toDouble, values(7))

    val identsByScan = idents.groupBy(_.scan).map { case (k, v) => k -> v.head }

    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"

    val fw = new BufferedWriter(new FileWriter(new File((new File(mzdbFilePath)).getParentFile, "precursors_stats_full_v3_6.txt" )))
    fw.write(annotations.mkString("\t"))
    fw.newLine()

    val precComputer = new IsolationWindowPrecursorExtractor_v3_6(mzTol)

    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    mzDbReader.enablePrecursorListLoading()
    mzDbReader.enableScanListLoading()

    logger.info("nb identifications = {}", idents.length)
    logger.info("nb MS2 scans = {}", mzDbReader.getSpectraCount(2))

    for( (scan, identification) <- identsByScan  ) {

      val spectrumHeader = mzDbReader.getSpectrumHeader(scan)
      var map = precComputer.extractPrecursorStats(mzDbReader, spectrumHeader, identification.moz, mzTol)
      map += ("scan.number" -> scan)
      map += ("scan.rt" -> spectrumHeader.getElutionTime/60.0)
      map += ("ident.status" -> identification.status)
      map += ("ident.moz" -> identification.moz)
      map += ("ident.charge" -> identification.charge)
      map += ("ident.score_max" -> identification.scoreMax)
      map += ("ident.score_from" -> identification.scoreFrom)

      dumpStats(map, fw)
    }


    logger.info(metric.toString());
    precComputer.dumpMetrics()
    fw.flush
    fw.close
    ()
  }


}
