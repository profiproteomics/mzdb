package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.io.reader.iterator.SpectrumIterator
import fr.profi.mzdb.io.writer.mgf.{AnnotatedMgfPrecursor, IsolationWindowPrecursorExtractor_v3_7}
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

@Ignore
/**
 *   Search the moz of the identified precursors in the mzdb file by targeting the scan number and extract some metrics about
 *   signal found in the raw data. The extracted stats are dumped into a txt file.
 */
class PrecursorGenerationTest extends StrictLogging {

  private val metric = new Metric("PrecursorGenerationTest")

  @Test
  def testMGFGeneration() = {

    val mzTol = 10.0f
    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"
    val precComputer = new IsolationWindowPrecursorExtractor_v3_7(mzTol)

    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    mzDbReader.enablePrecursorListLoading()
    mzDbReader.enableScanListLoading()

    logger.info("nb MS2 scans = {}", mzDbReader.getSpectraCount(2))

    // Iterate MSn spectra
    val spectrumIterator = new SpectrumIterator(mzDbReader, mzDbReader.getConnection, 2)
    while (spectrumIterator.hasNext) {

      val spectrumHeader = spectrumIterator.next.getHeader
      val mgfPrecursors = precComputer.getPossibleMgfPrecursorsFromSW(mzDbReader, spectrumHeader)

      metric.incr("nb_precursors_generated."+ mgfPrecursors.size)
      var rank = 0
      mgfPrecursors.foreach { mgfPrecursor =>
        metric.incr("mgf.entry")
        if (rank == 0) {
          metric.incr("first.was_rank"+mgfPrecursor.asInstanceOf[AnnotatedMgfPrecursor].getAnnotation("rank"))
        }
        rank = rank + 1
      }

    }

    logger.info(metric.toString());
    ()
  }


}
