package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import fr.profi.mzdb.db.model.params.param.CVEntry
import fr.profi.util.metrics.Metric
import org.junit.{Ignore, Test}

@Ignore
class HeadersTest extends StrictLogging {

  private var metric = new Metric("HeadersTest")


  @Test
  def testHeaders() = {
    val start = System.currentTimeMillis()

    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/Exploris/Xpl1_002790.mzDB"
//    val mzdbFilePath = "C:/Local/bruley/Data/Proline/Data/mzdb/TMT/HF1_012692.mzdb"

    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    mzDbReader.enablePrecursorListLoading()
    mzDbReader.enableScanListLoading()


    mzDbReader.getMs2SpectrumHeaders.foreach { sh =>

      if (sh.getScanList == null) sh.loadScanList(mzDbReader.getConnection)

      metric.incr("ms2_scan")

      val headerMz = sh.getPrecursorMz
      val precursorMz = sh.getPrecursorMz

      val iw = sh.getPrecursor.getIsolationWindow

      val cvEntries = Array[CVEntry](CVEntry.ISOLATION_WINDOW_LOWER_OFFSET, CVEntry.ISOLATION_WINDOW_TARGET_MZ, CVEntry.ISOLATION_WINDOW_UPPER_OFFSET)
      val cvParams = iw.getCVParams(cvEntries)

      val lowerMzOffset = cvParams(0).getValue.toFloat
      val targetMz = cvParams(1).getValue.toFloat
      val upperMzOffset = cvParams(2).getValue.toFloat


      val precMzParam = sh.getScanList.getScans.get(0).getUserParam("[Thermo Trailer Extra]Monoisotopic M/Z:")
      val refinedPrecMz = precMzParam.getValue.toDouble
      if (refinedPrecMz <= 0.0) {
        metric.incr("no_thermo_mz_value")
      }


//      logger.info(s"scan=${sh.getId}, h.mz=${headerMz}, p.mz=${precursorMz}, w.mz=${targetMz}, t.mz=${refinedPrecMz}")

//      if (math.abs(1e6*(headerMz - precursorMz)/headerMz) > 10.0) {
        metric.addValue("header_prec", math.abs(1e6*(headerMz - precursorMz)/headerMz))
//      }

      if (math.abs(1e6*(headerMz - targetMz)/headerMz) > 10.0) {
        metric.addValue("header_target", math.abs(1e6*(headerMz - targetMz)/headerMz))
      }

//      if (math.abs((headerMz - targetMz)) > 1.1) {
        logger.info(s"scan=${sh.getId}; header.mz=${headerMz}; precursor.mz=${precursorMz}; iw_center.mz=${targetMz}; thermo.mz=${refinedPrecMz}; z=${sh.getPrecursorCharge}+")
//      }


      //      if ( (refinedPrecMz > 0.0) && (math.abs(1e6*(headerMz - refinedPrecMz)/headerMz) > 10.0)) {
      if ( (refinedPrecMz > 0.0) ) {
        metric.addValue("header_thermo", math.abs(1e6*(headerMz - refinedPrecMz)/headerMz))
      }

    }

    mzDbReader.close()

    val took = (System.currentTimeMillis - start) / 1000f
    logger.info("extraction took: " + took)

    logger.info(metric.toString())
    ()
  }


}
