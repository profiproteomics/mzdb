package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.io.reader.iterator.SpectrumIterator
import fr.profi.mzdb.io.writer.mgf.{AnnotatedMgfPrecursor, IsolationWindowPrecursorExtractor_v3_6, MgfPrecursor}
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

import java.io.BufferedWriter
import scala.io.Source

@Ignore
/**
 *   Compare the Precursor's list generated from the mzdb file with the annotated identified scans
 */
class GeneratedPrecursorsVsIdentificationsTest_newIdentList extends StrictLogging {

  private val CONCORDANT = "concordant"
  private val UP_QX = "up QX"
  private val UP_MZDB = "up mzdb"

  private var metric = new Metric("GeneratedPrecursorsVsIdentificationsTest")

  case class Identification(scan: Int, bestQuery: Long, bestScore: Double, bestMoz:Double, bestCharge:Int, bestDecoyCount: Int = 0, bestTargetCount: Int = 0, bestTDCount: Int = 0) {

    def isTarget(): Boolean = {
      (bestTargetCount > 0) && (bestDecoyCount == 0) && (bestTDCount == 0)
    }

    def isDubious(): Boolean = {
      (bestTDCount > 0) || ((bestTargetCount > 0) && (bestDecoyCount > 0))
    }

    def isDecoy(): Boolean = {
      (bestDecoyCount > 0) && (bestTargetCount == 0) && (bestTDCount == 0)
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
  }


  def dumpMGFPrecursor(mgfPrecursor: MgfPrecursor, fw : BufferedWriter) = {
      val annotations = Array("scan.number", "source", "prediction", "initialPeak", "maxPeak", "rank")
      val prec = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor]
      val annotationValues = annotations.map{ k => prec.getAnnotationOrElse(k, "") }
      val values = Array(prec.getPrecMz, prec.getCharge, prec.getRt) ++ annotationValues
      fw.write(values.mkString("\t"))
      fw.newLine()
  }


  @Test
  def testMGFGeneration() = {

    val mzTol = 10.0f

    val lines = Source.fromInputStream(GeneratedPrecursorsVsIdentificationsTest_newIdentList.this.getClass.getResourceAsStream("/v3.6_list.csv")).getLines().drop(1).toSeq
    val idents = for {line <- lines
                      values = line.split(";").map(_.trim)}
    yield Identification(values(0).toInt, values(1).toLong, values(2).toDouble, values(3).toDouble, values(4).toInt, values(5).toInt, values(6).toInt, values(7).toInt)


    val identsByScan = idents.groupBy(_.scan).map { case (k, v) => k -> v.head }
    val matches = collection.mutable.Set.empty[Int]
    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"

//    val fw = new BufferedWriter(new FileWriter(new File((new File(mzdbFilePath)).getParentFile, "precursors.txt" )))

    val precComputer = new IsolationWindowPrecursorExtractor_v3_6(mzTol)
    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    mzDbReader.enablePrecursorListLoading()
    mzDbReader.enableScanListLoading()

    logger.info("nb identifications = {}", idents.length)
    logger.info("nb MS2 scans = {}", mzDbReader.getSpectraCount(2))


    precComputer.getMgfPrecursors(mzDbReader, mzDbReader.getSpectrumHeader(116538))


    // Iterate MSn spectra
    val spectrumIterator = new SpectrumIterator(mzDbReader, mzDbReader.getConnection, 2)
    while (spectrumIterator.hasNext) {

      val spectrumHeader = spectrumIterator.next.getHeader
      val mgfPrecursors = precComputer.getMgfPrecursors(mzDbReader, spectrumHeader)

      var rank = 0
      mgfPrecursors.foreach { mgfPrecursor =>

        metric.incr("mgf.entry")

        val mgfMz = mgfPrecursor.getPrecMz //precComputer.getPrecursorMz(mzDbReader, spectrumHeader)
        val mgfZ = mgfPrecursor.getCharge //precComputer.getPrecursorCharge(mzDbReader, spectrumHeader)

        if (identsByScan.contains(spectrumHeader.getInitialId)) {
          val ident = identsByScan(spectrumHeader.getInitialId)
          if ((ident.bestCharge == mgfZ) && (Math.abs(1e6 * (ident.bestMoz - mgfMz) / mgfMz) < mzTol)) {

            if (matches.contains(spectrumHeader.getInitialId )) {
              metric.incr("match.duplicated")
            } else {
              matches += spectrumHeader.getInitialId
              metric.incr("match")

            }
          }
        }
        rank = rank + 1
      }

      // count number of precursors per spectrum

      metric.incr("nb_precursors_generated."+ mgfPrecursors.size)

      // test precursor list generated from the current spectrumId

      if (identsByScan.contains(spectrumHeader.getInitialId)) {
        val ident = identsByScan(spectrumHeader.getInitialId)
        val matching = mgfPrecursors.find(prec => (ident.bestCharge == prec.getCharge) && (Math.abs(1e6 * (ident.bestMoz - prec.getPrecMz) / prec.getPrecMz) < mzTol))
        if (!matching.isDefined) {
          metric.incr("lost_match")

        } else {
          // for matched identifications, count how many precursors are generated
          metric.incr("matched.nb_precursors_generated."+ mgfPrecursors.size)
        }
      }

    }

    logger.info(metric.toString());
    precComputer.dumpMetrics()
//    fw.flush
//    fw.close
    ()
  }


}
