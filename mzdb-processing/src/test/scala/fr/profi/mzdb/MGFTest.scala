package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.io.reader.iterator.SpectrumIterator
import fr.profi.mzdb.io.writer.mgf.{AnnotatedMgfPrecursor, IsolationWindowPrecursorExtractor_v3_7, MgfPrecursor}
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

import java.io.BufferedWriter
import scala.io.Source

@Ignore
class MGFTest extends StrictLogging {

  private val CONCORDANT = "concordant"
  private val UP_QX = "up QX"
  private val UP_MZDB = "up mzdb"

  private var metric = new Metric("MGFTest")

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

    val lines = Source.fromInputStream(MGFTest.this.getClass.getResourceAsStream("/run_2790.csv")).getLines().drop(1).toSeq
    val idents = for {line <- lines
                      values = line.split(";").map(_.trim)}
    yield Identification(values(0).split("_")(1).toInt, values(1).toDouble, values(2).toInt, values(3).toDouble, values(4).toInt, values(5))

    logger.info("nb identifications = {}", idents.length)
    val identsByScan = idents.groupBy(_.scan).map { case (k, v) => k -> v.head }

    val matches = collection.mutable.Set.empty[Int]

    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"

//    val fw = new BufferedWriter(new FileWriter(new File((new File(mzdbFilePath)).getParentFile, "precursors.txt" )))

    val precComputer = new IsolationWindowPrecursorExtractor_v3_7(mzTol)

    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    mzDbReader.enablePrecursorListLoading()
    mzDbReader.enableScanListLoading()


//    val spectrum = mzDbReader.getSpectrum(18077)
//    val precs = precComputer.getMgfPrecursors(mzDbReader, spectrum.getHeader)


    // Iterate MSn spectra
    val spectrumIterator = new SpectrumIterator(mzDbReader, mzDbReader.getConnection, 2)

    while (spectrumIterator.hasNext) {


      val spectrumHeader = spectrumIterator.next.getHeader

//      logger.info("generating (mz, z) for scan {} ...", spectrumHeader.getInitialId)

      val mgfPrecursors = precComputer.getMgfPrecursors(mzDbReader, spectrumHeader)

      var rank = 0
      mgfPrecursors.foreach { mgfPrecursor =>

//        dumpMGFPrecursor(mgfPrecursor, fw)
        
        metric.incr("mgf.entry")

        val mgfMz = mgfPrecursor.getPrecMz //precComputer.getPrecursorMz(mzDbReader, spectrumHeader)
        val mgfZ = mgfPrecursor.getCharge //precComputer.getPrecursorCharge(mzDbReader, spectrumHeader)


        if (identsByScan.contains(spectrumHeader.getInitialId)) {
          val ident = identsByScan(spectrumHeader.getInitialId)

          if ((ident.charge == mgfZ) && (Math.abs(1e6 * (ident.moz - mgfMz) / mgfMz) < mzTol)) {

            if (matches.contains(spectrumHeader.getInitialId )) {
//              logger.info("The spectrum  {} was already matched", spectrumHeader.getInitialId)
              metric.incr("match.duplicated")
            } else {
              matches += spectrumHeader.getInitialId
              metric.incr("match")
              ident.status match {
                case CONCORDANT => metric.incr("match.was_concordant")
                case UP_QX => metric.incr("match.was_up_QX")
                case UP_MZDB => metric.incr("match.was_up_mzdb")
              }

              if (ident.status.equals(UP_QX) && rank >= 2) {
                metric.incr("match.rank_n")
//                val peak = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("initialPeak").asInstanceOf[Peak]
//                val maxPeak = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("maxPeak").asInstanceOf[Peak]
//                val initialRank = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("rank").asInstanceOf[Integer]
//                val ratio = 100*peak.getIntensity/maxPeak.getIntensity
//                metric.addValue("rank_n.intensity", ratio)
//                metric.addValue("rank_n.initial_rank", initialRank.doubleValue())

//                logger.info("rank > 3 match : Scan {}, mz = {}, z = {}, mgf_mz = {}, mgf_z = {},rank = {},  initrank = {}, ratio = {}", spectrumHeader.getInitialId, ident.moz, ident.charge, mgfMz, mgfZ, rank, initialRank, ratio)
              }

            }


          } else {

//            if (Math.abs(1e6 * (ident.moz - mgfMz) / mgfMz) > mzTol) {
//              metric.incr("non_match.incorrect_prec_mz")
//              if (Math.abs(1e6 * (ident.moz - spectrumHeader.getPrecursorMz) / ident.moz) < mzTol) {
//                metric.incr("non_match.header_prec_mz_was_correct")
//              }
//            }

          }

          //        if (ident.status.equals("concordant")) {
          //          logger.info("Scan {}, mz = {}, z = {}, mgf_mz = {}, mgf_z = {}", spectrumHeader.getInitialId, ident.moz, ident.charge, mgfMz, mgfZ)
          //        } else {
          //          logger.info("non concordant")
          //        }
          //

        } else {
          //        logger.info("scan {} not found in identifications", spectrumHeader.getInitialId)
          metric.incr("scan_not_found")
        }
        rank = rank + 1
      }

        if (identsByScan.contains(spectrumHeader.getInitialId)) {
          val ident = identsByScan(spectrumHeader.getInitialId)
          val matching = mgfPrecursors.find(prec => (ident.charge == prec.getCharge) && (Math.abs(1e6 * (ident.moz - prec.getPrecMz) / prec.getPrecMz) < mzTol))
          if (!matching.isDefined) {
            metric.incr("non_match")

            if (ident.status.equals(CONCORDANT) || ident.status.equals(UP_MZDB)) {
              metric.incr("lost matches")
              if (Math.abs(1e6 * (ident.moz - spectrumHeader.getPrecursorMz) / spectrumHeader.getPrecursorMz) < mzTol) {
                metric.incr("lost_matches.header_prec_mz_was_correct")
//                if (ident.charge == spectrumHeader.getPrecursorCharge) {
//                  logger.info("exemple of lost match: {}, {}, {}", ident.scan, ident.moz, ident.charge)
//                }
              }
            }

            if (ident.status.equals(CONCORDANT)) {
              metric.incr("lost matches.was_concordant")
            } else if (ident.status.equals(UP_MZDB)) {
              metric.incr("lost matches.was_up_mzdb")
            } else if (ident.status.equals(UP_QX)) {
              metric.incr("lost matches.was_up_qx")
//              val bestMatch = mgfPrecursors.minBy(prec => (Math.abs(1e6 * (ident.moz - prec.getPrecMz) / prec.getPrecMz)))

              //logger.info("Up QX Scan {}; {}; {}; {}; {}", spectrumHeader.getInitialId, ident.moz, ident.charge, bestMatch.getPrecMz, bestMatch.getCharge)
            }
          } else {
            // there is a match for this prec
            val matchingPrec = matching.get
            if (matchingPrec == mgfPrecursors.head) {
//              // the first one is matching
//              val unused = mgfPrecursors.tail
//              unused.foreach{ mgfPrecursor =>
//                val peak = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("initialPeak").asInstanceOf[Peak]
//                val maxPeak = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("maxPeak").asInstanceOf[Peak]
//                val initialRank = mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("rank").asInstanceOf[Integer]
//                val ratio = 100*peak.getIntensity/maxPeak.getIntensity
//                metric.addValue("unused.intensity", ratio)
//                metric.addValue("unused.initial_rank", initialRank.doubleValue())
//              }
            }
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
