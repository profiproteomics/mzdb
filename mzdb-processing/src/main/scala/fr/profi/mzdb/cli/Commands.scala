package fr.profi.mzdb.cli

import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb._
import fr.profi.mzdb.algo.feature.extraction.FeatureExtractorConfig
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.waveletImpl.WaveletDetectorDuMethod
import fr.profi.mzdb.io.exporter.SQLiteFeatureStorer
import fr.profi.mzdb.io.reader.provider.RunSliceDataProvider
import fr.profi.mzdb.io.writer.mgf._
import fr.profi.mzdb.model._
import fr.profi.util.stat.EntityHistogramComputer
import mr.go.sgfilter.SGFilterMath3

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


/**
 * @author David Bouyssie
 *
 */
object Commands extends LazyLogging {

  def generateXICs(
    mzdbListFilePath: String,
    pepListFilePath: String,
    outputFilePath: String,
    mzTolInPPM: Float,
    algo: String
  ) {

    case class DetectedPeak(mz: Double, apex: Peak, duration: Float, area: Float) {
      lazy val intensity: Float = apex.getIntensity()
      lazy val time: Float = apex.getLcContext().getElutionTime()

      def getApexFullSpectrumIntensity(mzDb: MzDbReader): Float = {
        val spectrum = mzDb.getSpectrum(apex.getLcContext().getSpectrumId())
        spectrum.getHeader().getTIC()
      }

    }

    // Define some helper functions
    //def peakTime(p: Peak): Float = p.getLcContext().getElutionTime()
    def sumPeaks(peaks: Seq[Peak]): Float = peaks.foldLeft(0f)((s, p) => s + p.getIntensity())
    def highestPeakIntensity(peaks: Seq[Peak]): Float = peaks.sortWith((a, b) => a.getIntensity() > b.getIntensity()).head.getIntensity()

    def getMzdbList(): Array[String] = {
      val a = Source.fromFile(mzdbListFilePath).getLines.toArray
      this.logger.info(s"Found #${a.length} files to analyze")
      a
    }

    def getPeptideList(pepFile: File): Array[Tuple3[Double, Float, Float]] = {
      val mzList = Source.fromFile(pepFile.getAbsolutePath()).getLines.map { l =>
        val splitted = "\\s".r.split(l)
        var returnVal: Tuple3[Double, Float, Float] = null;

        if (splitted.length == 1)
          returnVal = new Tuple3[Double, Float, Float](splitted(0).toDouble, 0f, 0f)
        else if (splitted.length == 3) {
          val mz = splitted(0).toDouble
          var (rtmin, rtmax) = (splitted(1).toFloat, splitted(2).toFloat)
          if (rtmin > rtmax) {
            logger.warn("rtmin is bigger than rtmax: switching both...")
            val tmp = rtmin
            rtmin = rtmax
            rtmax = tmp
          }
          returnVal = new Tuple3[Double, Float, Float](mz, rtmin, rtmax)
        } else {
          throw new Exception("can not parse correctly pepList file, line:" + l)
        }
        returnVal
      }.toArray
      mzList
    }

    def extractXICs(mzDb: MzDbReader, mzList: Array[Tuple3[Double, Float, Float]]): Array[Tuple2[Double, Option[DetectedPeak]]] = {

      // extract xics NOT in parallel sqlite4java fails
      val peakMatrix = mzList.map { case (mz, rtmin, rtmax) =>
        val mzTolInDa = mzTolInPPM * mz / 1e6
        val (minMz, maxMz) = (mz - mzTolInDa, mz + mzTolInDa)
        val peaks = mzDb.getMsXicInMzRange(minMz, maxMz, XicMethod.MAX)
        this.logger.info(s"XIC for mass ${mz} contains #${peaks.length} peaks")
        (mz, peaks, (rtmin, rtmax))
      }

      if (algo == "basic") this.logger.info("basic algorithm")
      else if (algo == "wavelet") this.logger.info("wavelet algorithm")
      else throw new Exception("Specified algorithm is not defined:" + algo)

      // peaks detection on xics on parallel
      peakMatrix.par.map {
        case (mz, peaks, (rtmin, rtmax)) =>
          var peakelIndexes: Array[(Int, Int)] = null
          if (algo == "basic") {
            val basicPeakelFinder = new BasicPeakelFinder()
            peakelIndexes = basicPeakelFinder.findPeakelsIndices(peaks);
          } else if (algo == "wavelet") {
            val wpf = new WaveletDetectorDuMethod(peaks)
            wpf.ridgeFilteringParams.minSNR = 0.0f
            peakelIndexes = wpf.findPeakelsIndexes()
          }
          // Retrieve the peakel corresponding to the feature apex
          val peakels = if (rtmin == 0f && rtmax == 0f) {
            peakelIndexes.map { peakelIdx => (peakelIdx._1 to peakelIdx._2).toArray.map(peaks(_)) }
          } else {
            peakelIndexes
              .withFilter { x =>
                peaks(x._1).getLcContext().getElutionTime() / 60.0 > rtmin &&
                  peaks(x._2).getLcContext().getElutionTime() / 60.0 < rtmax
              }
              .map { peakelIdx =>
                (peakelIdx._1 to peakelIdx._2).toArray.map(peaks(_))
              }
          }

          val sortedPeakels = peakels.sortWith((a, b) => highestPeakIntensity(a) > highestPeakIntensity(b))

          val intSum = if (sortedPeakels.length > 0) {

            // Retrieve the apex
            val peaks = sortedPeakels(0)
            val apex = peaks.reduce((a, b) => if (a.getIntensity() > b.getIntensity()) a else b)

            // Smooth peaks
            //val intensities = peaks.map( p => Option(p).map(_.getIntensity).getOrElse(0f) )
            //val smoothedIntensities = smoothIntensities( intensities )
            /*val smoothedPeaks = peaks.zip(smoothedIntensities).map { case (p,si) =>
            val sp = new Peak(p.getMz(),si)
            sp.setLcContext(p.getLcContext())
            sp
          }*/

            val apexIdx = peaks.indexOf(apex)

            val (leftPeaksWithIdx, rightPeaksWithIdx) = peaks.zipWithIndex.partition { case (p, i) => i <= apexIdx }

            def keepPeaksWithIncreasingIntensity(peaksWithIdx: Array[(Peak, Int)]): Array[(Peak, Int)] = {

              var prevIntensity = 0f
              peaksWithIdx.filter {
                case (p, i) =>
                  val curIntensity = p.getIntensity
                  val isHigher = curIntensity >= prevIntensity
                  prevIntensity = curIntensity
                  isHigher
              }

            }

            val leftPeaksFiltered = keepPeaksWithIncreasingIntensity(leftPeaksWithIdx)
            val rightPeaksFiltered = keepPeaksWithIncreasingIntensity(rightPeaksWithIdx.reverse)
            val smoothedPeaks = for ((p, i) <- leftPeaksFiltered ++ rightPeaksFiltered.reverse) yield p

            // Filter the peaks
            val threshold = apex.getIntensity() * 0.05
            val filteredPeaks = smoothedPeaks.filter(p => p.getIntensity() > threshold)

            // Build a peakel
            val peakel = new PeakelBuilder(filteredPeaks).result() //.map(Some(_)))
            /*
          // Compute the duration
          val duration = peakTime(filteredPeaks.last) - peakTime(filteredPeaks.head)
          
          apex.getMz(), peakTime(apex), duration, sumPeaks(filteredPeaks),
          */
            mz -> Some(DetectedPeak(peakel.getMz, apex, peakel.calcDuration, peakel.area))
          } else mz -> None

          intSum
      }.toArray

    } //ends function

    def extractXICsForOneFile(mzdbFilePath: String, mzList: Array[(Double, Float, Float)]): Array[String] = {
      val start = System.currentTimeMillis()

      var mzDb: MzDbReader = null
      try {
        val mzDb = new MzDbReader(mzdbFilePath, true)
        val xicsBuilder = new ArrayBuffer[String]

        // Output the results
        val peptides = extractXICs(mzDb, mzList) //Array(mzList(1))
        for (pep <- peptides) {
          val mz = pep._1
          if (pep._2.isDefined) {
            val peak = pep._2.get
            val (expMz, time, dur, area, int, msSpectrumInt) = (
              peak.mz,
              peak.time / 60,
              peak.duration / 60,
              peak.area,
              peak.intensity,
              peak.getApexFullSpectrumIntensity(mzDb)
            )
            xicsBuilder += List(mzdbFilePath, mz, expMz, time, dur, area, int, msSpectrumInt).mkString("\t")

            //writer.println(List(mzdbFilePath, mz,expMz,time,dur,area,int,msSpectrumInt).mkString("\t"))
            logger.info("found peptide ion of intensity |" + int + "| and duration |" + dur + "| at |" + time + "|")
          } else logger.warn("can't find peptide ion of m/z=" + mz)
        }

        val took = (System.currentTimeMillis - start) / 1000f
        this.logger.info("extraction took: " + took)
        xicsBuilder.toArray

      } catch {
        case e: Exception => {
          this.logger.error("Extraction failed for file " + mzdbFilePath + " because: " + e.getMessage())
          //e.printStackTrace()
          Array.empty[String]
        }
      } finally {
        if (mzDb != null)
          mzDb.close()
      }

    } //end function

    val mzList = getPeptideList(new File(pepListFilePath))

    val writer = new PrintWriter(new FileOutputStream(outputFilePath))
    writer.println(List("file name", "input m/z", "exp m/z", "time", "duration", "area", "fwhm", "intensity", "ms1_intensity").mkString("\t"))
    writer.flush()

    this.logger.info("Starting the extraction of XICs...")
    val xics = getMzdbList().map(extractXICsForOneFile(_, mzList))
    xics.flatMap(x => x).foreach(writer.println(_))
    writer.close()

    this.logger.info("Extraction of XICs finished !")
  }

  def extractFeatures(mzdbFilePath: String, putativeFtsFile: String, outputFile: String) {

    def getPutativeFeatures(): Array[PutativeFeature] = {
      Source.fromFile(putativeFtsFile).getLines.map { l =>
        val splitted = "\\s".r.split(l)
        new PutativeFeature(
          id = Feature.generateNewId(),
          mz = splitted(0).toDouble,
          charge = splitted(1).toInt,
          elutionTime = splitted(2).toFloat,
          evidenceMsLevel = 1,
          isPredicted = true)
      }.toArray
    }

    val mzDb = new MzDbReader(new java.io.File(mzdbFilePath), true)

    logger.info("Hello Predicted Time Extractor")

    val pfs = getPutativeFeatures()
    this.logger.info(s"Read #${pfs.length} putative features...")

    val rsIter = mzDb.getLcMsRunSliceIterator()
    logger.info("got RS iterator")

    val rsdProv = new RunSliceDataProvider(rsIter)
    val mzTolPPM = 10

    logger.info("Starting Extraction...")
    val mzdbFtX = new MzDbFeatureExtractor(mzDb, 5, 2, FeatureExtractorConfig(mzTolPPM = 10, maxIPDeviation = Some(40f)))
    val xFts = mzdbFtX.extractFeatures(rsdProv, pfs, mzTolPPM)

    logger.info("Storing detected features")
    SQLiteFeatureStorer.storeFeatures(xFts, new File(outputFile))

    logger.info("Ended")

  }

  protected def smoothIntensities(intensities: Array[Float]): Array[Float] = {
    val times = 3

    // TODO: static values
    val (nl, nr, order) = (5, 5, 4)
    val coeffs = SGFilterMath3.computeSGCoefficients(nl, nr, order)

    val sgFilter = new SGFilterMath3(5, 5)
    var smoothedIntensities = intensities
    for (i <- 1 to times) {
      smoothedIntensities = sgFilter.smooth(smoothedIntensities, coeffs)
    }

    smoothedIntensities
  }

  /**
   *
   */
  def dumpRegion(mzdbFilePath: String, outputFilePath: String, mzmin: Double, mzmax: Double, rtmin: Float, rtmax: Float) {

    val start = System.currentTimeMillis()

    val mzDb = new MzDbReader(mzdbFilePath, true)
    val outStream = new PrintWriter(new FileOutputStream(outputFilePath))
    val spectrumSlices = mzDb.getMsSpectrumSlices(mzmin, mzmax, rtmin, rtmax) // always in mslevel 1 by default

    outStream.println(List("moz", "time", "intensity").mkString("\t"))

    spectrumSlices.foreach(spectrumSlice =>
      spectrumSlice.toPeaks().foreach(peak =>
        outStream.println(List(peak.getMz(), peak.getLcContext().getElutionTime(), peak.getIntensity()).mkString("\t"))
      )
    )

    outStream.close()
    mzDb.close()

    val took = (System.currentTimeMillis - start) / 1000f
    logger.info("extraction took: " + took)
  }

  def dumpRegionBinning(mzdbFilePath: String, outputFilePath: String, nbBins: Int, mzmin: Double, mzmax: Double, rtmin: Float, rtmax: Float) {

    val start = System.currentTimeMillis()

    val mzDb = new MzDbReader(mzdbFilePath, true)
    val outStream = new PrintWriter(new FileOutputStream(outputFilePath))
    val spectrumSlices = mzDb.getMsSpectrumSlices(mzmin, mzmax, rtmin, rtmax)
    val flattenedPeaksMz = spectrumSlices.map(_.toPeaks).flatten.map(_.getMz).sortBy(x => x)
    val (minmz, maxmz) = (flattenedPeaksMz.head, flattenedPeaksMz.last)

    val binner = new EntityHistogramComputer(flattenedPeaksMz, (x: Double) => x)
    val mzBins = binner.calcHistogram(nbBins)

    val mzList = mzBins.map { case (bin, values) => bin.center }.toArray
    val rtList = spectrumSlices.map(_.getHeader().getElutionTime()).toArray
    val intList = Array.ofDim[Float](spectrumSlices.length, mzBins.length)

    var i = 0
    spectrumSlices.foreach { spectrumSlice =>
      val binner_ = new EntityHistogramComputer(spectrumSlice.toPeaks, (x: Peak) => x.getMz())
      val bins = binner_.calcHistogram(nbBins, range = Some(Tuple2(minmz, maxmz)))
      var j = 0
      bins.foreach {
        case (bin, values) =>
          intList(i)(j) = values.map(_.getIntensity).sum
          j += 1
      }
      i += 1
    }
    outStream.println("moz: " + mzList.mkString("\t"))
    outStream.println("rt: " + rtList.mkString("\t"))
    val intString = intList.map(_.mkString("\t")).toArray.mkString(",")
    outStream.println("intensities: " + intList.map(_.mkString("\t")).toArray.mkString(","))

    outStream.close()
    mzDb.close()

    val took = (System.currentTimeMillis - start) / 1000f
    logger.info("extraction took: " + took)
  }

  def dumpScanHeaders(mzdbFilePath: String, outputFilePath: String, msLevel: Int) {

    val start = System.currentTimeMillis()

    val mzDbReader = new MzDbReader(mzdbFilePath, true)
    val outStream = new PrintWriter(new FileOutputStream(outputFilePath))

    if (msLevel == 2) {
      outStream.println(List("file", "scan", "moz", "charge").mkString("\t"))
      mzDbReader.getMs2SpectrumHeaders.foreach(sh =>
          outStream.println(List(mzdbFilePath, sh.getInitialId, sh.getPrecursorMz, sh.getPrecursorCharge).mkString("\t"))
      )
    } else {
      throw new UnsupportedOperationException("Not yet implemented")
    }

    outStream.close()
    mzDbReader.close()

    val took = (System.currentTimeMillis - start) / 1000f
    logger.info("extraction took: " + took)
  }

  // TODO: extract command parameters in method signature ?
  def createMgf(): Unit = {
    
    import fr.profi.mzdb.cli.MzDbProcessing.CreateMgfCommand

    logger.info("Creating MGF File for mzDB at: " + CreateMgfCommand.mzdbFile)
    logger.info("Precursor m/z values will be defined using the method: " + CreateMgfCommand.precMzComputation)

    val writer = new MgfWriter(CreateMgfCommand.mzdbFile, CreateMgfCommand.msLevel)
    val precCompEnum = PrecursorMzComputationEnum.values().find(_.name() == CreateMgfCommand.precMzComputation.toUpperCase)
    val specProcessor = new DefaultSpectrumProcessor

    if (precCompEnum.isDefined) {
      writer.write(CreateMgfCommand.outputFile,  new DefaultPrecursorComputer(precCompEnum.get, CreateMgfCommand.mzTolPPM), specProcessor, CreateMgfCommand.intensityCutoff, CreateMgfCommand.exportProlineTitle)
    } else if (CreateMgfCommand.precMzComputation == "isolation_window_extracted") {
      val precComputer = new IsolationWindowPrecursorExtractor(CreateMgfCommand.mzTolPPM)
      writer.write(CreateMgfCommand.outputFile, precComputer, specProcessor, CreateMgfCommand.intensityCutoff, CreateMgfCommand.exportProlineTitle)
    } else if (CreateMgfCommand.precMzComputation == "isolation_window_extracted_v3.6") {
      val precComputer = new IsolationWindowPrecursorExtractor_v3_6(CreateMgfCommand.mzTolPPM)
      writer.write(CreateMgfCommand.outputFile, precComputer, specProcessor, CreateMgfCommand.intensityCutoff, CreateMgfCommand.exportProlineTitle)
    } else if (CreateMgfCommand.precMzComputation == "isolation_window_extracted_v3.7") {
      val precComputer = new IsolationWindowPrecursorExtractor_v3_7(CreateMgfCommand.mzTolPPM)
      writer.write(CreateMgfCommand.outputFile, precComputer, specProcessor, CreateMgfCommand.intensityCutoff, CreateMgfCommand.exportProlineTitle)
    }  else {
      throw new IllegalArgumentException("Can't create the MGF file, invalid precursor m/z computation method")
    }
  }

}