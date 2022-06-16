package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.io.writer.mgf.IsolationWindowPrecursorExtractor_v3_6
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

@Ignore
class SearchIdentifiedPrecursorsTest_newIdentList extends StrictLogging {

  private val metric = new Metric("SearchIdentifiedPrecursorsTest_newIdentList")
  private val annotations = Array("scan.number", "scan.rt", "ident.status", "ident.duplicated.status", "ident.moz", "ident.charge", "ident.score_max", "found",
    "ident.initial.rank", "ident.initial.moz" , "ident.initial.intensity", "ident.prediction.moz", "ident.prediction.charge", "ident.prediction.note", "ident.prediction.intensity", "ident.prediction.rank",
    "rank0.initial.intensity", "rank0.initial.moz", "rank0.prediction.moz", "rank0.prediction.charge", "rank0.prediction.note", "rank0.prediction.rank",
    "swcenter.initial.rank", "swcenter.initial.moz", "swcenter.initial.intensity", "swcenter.prediction.moz", "swcenter.prediction.charge", "swcenter.prediction.note", "swcenter.prediction.rank",
    "cause", "header.moz", "header.charge", "header.found", "header.prediction.note", "header.prediction.moz",  "header.prediction.charge", "sw_center.moz")

  case class Identification(scan: Int, bestQuery: Long, bestScore: Double, bestMoz:Double, bestCharge:Int, bestDecoyCount: Int = 0, bestTargetCount: Int = 0, bestTDCount: Int = 0,
      worstQuery: Long, worstScore: Double, worstMoz:Double, worstCharge:Int, worstDecoyCount: Int = 0, worstTargetCount: Int = 0, worstTDCount: Int = 0) {


    def isTarget(): Boolean = {
      (bestTargetCount > 0) && (bestDecoyCount == 0) && (bestTDCount == 0)
    }

    def isDubious(): Boolean = {
      (bestTDCount > 0) || ((bestTargetCount > 0) && (bestDecoyCount > 0))
    }

    def isDecoy(): Boolean = {
      (bestDecoyCount > 0) && (bestTargetCount == 0) && (bestTDCount == 0)
    }

    def isWorstTarget(): Boolean = {
      (worstTargetCount > 0) && (worstDecoyCount <= 0)  && (worstTDCount <= 0)
    }

    def isWorstDubious(): Boolean = {
      (worstTDCount > 0) || ((worstTargetCount > 0) && (worstDecoyCount > 0))
    }

    def isWorstDecoy(): Boolean = {
      (worstDecoyCount > 0) && (worstTargetCount <= 0) && (worstTDCount <= 0)
    }

    def status(): String = {
      if (isTarget()) {
        "TARGET"
      } else if (isDubious()) {
        "DUBIOUS"
      } else {
        "DECOY"
      }
    }

    def duplicatedStatus(): String = {
      if (isWorstTarget()) {
        "TARGET"
      } else if (isWorstDubious()) {
        "DUBIOUS"
      } else if (isWorstDecoy()) {
        "DECOY"
      } else {
        ""
      }
    }
  }

  def dumpStats(map : Map[String, Any], fw : BufferedWriter) = {

      val values = annotations.map{ k => map.getOrElse(k, "") }
      fw.write(values.mkString("\t"))
      fw.newLine()
  }

  def toInt(s: String):Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  def toLong(s: String):Option[Long] = {
    try {
      Some(s.toLong)
    } catch {
      case e: NumberFormatException => None
    }
  }

  def toDouble(s: String):Option[Double] = {
    try {
      Some(s.toDouble)
    } catch {
      case e: NumberFormatException => None
    }
  }

  @Test
  def testMGFGeneration() = {

    val mzTol = 10.0f

    val lines = Source.fromInputStream(SearchIdentifiedPrecursorsTest_newIdentList.this.getClass.getResourceAsStream("/v3.6_list.csv")).getLines().drop(1).toSeq
    val idents = for {line <- lines
                      values = line.split(";").map(_.trim)}
    yield Identification(values(0).toInt, values(1).toLong, values(2).toDouble, values(3).toDouble, values(4).toInt, values(5).toInt, values(6).toInt, values(7).toInt,
                         values(10).toLong, values(11).toDouble, values(8).toDouble, values(9).toInt, values(12).toInt, values(13).toInt, values(14).toInt)


    logger.info("nb identifications = {}", idents.length)
    val identsByScan = idents.groupBy(_.scan).map { case (k, v) => k -> v.head }

    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"

    val fw = new BufferedWriter(new FileWriter(new File((new File(mzdbFilePath)).getParentFile, "precursors_stats_full_v3_6_newList_expIsotope.txt" )))
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
      var map = precComputer.extractPrecursorStats(mzDbReader, spectrumHeader, identification.bestMoz, mzTol)
      map += ("scan.number" -> scan)
      map += ("scan.rt" -> spectrumHeader.getElutionTime/60.0)
      map += ("ident.status" -> identification.status)
      map += ("ident.duplicated.status" -> identification.duplicatedStatus())
      map += ("ident.moz" -> identification.bestMoz)
      map += ("ident.charge" -> identification.bestCharge)
      map += ("ident.score_max" -> identification.bestScore)

      dumpStats(map, fw)

      if (identification.worstQuery >= 0) {
        var map = precComputer.extractPrecursorStats(mzDbReader, spectrumHeader, identification.worstMoz, mzTol)
        map += ("scan.number" -> scan)
        map += ("scan.rt" -> spectrumHeader.getElutionTime/60.0)
        map += ("ident.status" -> identification.status())
        map += ("ident.duplicated.status" -> identification.duplicatedStatus())
        map += ("ident.moz" -> identification.worstMoz)
        map += ("ident.charge" -> identification.worstCharge)
        map += ("ident.score_max" -> identification.worstScore)

        dumpStats(map, fw)
      }
    }


    logger.info(metric.toString());
    precComputer.dumpMetrics()
    fw.flush
    fw.close
    ()
  }


}
