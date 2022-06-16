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
class GeneratedPrecursorsVsIdentificationsTest extends StrictLogging {

  private val CONCORDANT = "concordant"
  private val UP_QX = "up QX"
  private val UP_MZDB = "up mzdb"

  private var metric = new Metric("GeneratedPrecursorsVsIdentificationsTest")

  case class Identification(scan: Int, moz:Double, charge:Int, initMoz:Double, initCharge:Int, status:String)

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

    val lines = Source.fromInputStream(GeneratedPrecursorsVsIdentificationsTest.this.getClass.getResourceAsStream("/run_2790.csv")).getLines().drop(1).toSeq
    val idents = for {line <- lines
                      values = line.split(";").map(_.trim)}
    yield Identification(values(0).split("_")(1).toInt, values(1).toDouble, values(2).toInt, values(3).toDouble, values(4).toInt, values(5))

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
          if ((ident.charge == mgfZ) && (Math.abs(1e6 * (ident.moz - mgfMz) / mgfMz) < mzTol)) {

            if (matches.contains(spectrumHeader.getInitialId )) {
              metric.incr("match.duplicated")
            } else {
              matches += spectrumHeader.getInitialId
              metric.incr("match")
              ident.status match {
                case CONCORDANT => metric.incr("match.was_concordant")
                case UP_QX => metric.incr("match.was_up_QX")
                case UP_MZDB => metric.incr("match.was_up_mzdb")
              }

              if (ident.status.equals(UP_QX) && rank >= 1) {
                metric.incr("match.from_rank_n")
              }
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
        val matching = mgfPrecursors.find(prec => (ident.charge == prec.getCharge) && (Math.abs(1e6 * (ident.moz - prec.getPrecMz) / prec.getPrecMz) < mzTol))
        if (!matching.isDefined) {
          metric.incr("lost_match")

          if (ident.status.equals(CONCORDANT) || ident.status.equals(UP_MZDB)) {
            metric.incr("lost_match.from_legacy_mgf_generation")
            if (Math.abs(1e6 * (ident.moz - spectrumHeader.getPrecursorMz) / spectrumHeader.getPrecursorMz) < mzTol) {
              metric.incr("lost_match.from_legacy_mgf_generation.header_prec_mz_was_correct")
              //                if (ident.charge == spectrumHeader.getPrecursorCharge) {
              //                  logger.info("exemple of lost match: {}, {}, {}", ident.scan, ident.moz, ident.charge)
              //                }
            }
          }

          if (ident.status.equals(CONCORDANT)) {
            metric.incr("lost_match.was_concordant")
          } else if (ident.status.equals(UP_MZDB)) {
            metric.incr("lost_match.was_up_mzdb")
          } else if (ident.status.equals(UP_QX)) {
            metric.incr("lost_match.was_up_qx")
          }
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
