package fr.profi.mzdb.io.exporter

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.io.File

import scala.collection.mutable.ArrayBuffer

import org.jfree.chart._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data._
import org.jfree.data.xy._

import com.almworks.sqlite4java.SQLiteConnection
import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.algo.signal.detection.SmartPeakelFinder
import fr.profi.mzdb.algo.signal.filtering._
import fr.profi.mzdb.io.reader.ScanHeaderReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.DerivativeAnalysis

object SQLiteIsolationWindowStorer extends Logging {
  
  def storeIsolationWindows(mzDbReader: MzDbReader, dbLocation: File ) {

    val connection = new SQLiteConnection(dbLocation)
    connection.open(true) // allowsCreate = true
    
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=8000;")
  
      // Create tables
    connection.exec("CREATE TABLE isolation_window (" +
      " scan_id INTEGER,\n" +
      " time REAL,\n" +
      " precursor_mz REAL,\n" +
      " spectrum BLOB\n" +
      ")"
    )
    
    connection.exec("BEGIN TRANSACTION")
    
    // Prepare INSERT statement
    val stmt = connection.prepare("INSERT INTO isolation_window VALUES (?, ?, ?, ?)",true)
    
		// Configure the ScanHeaderReader in order to load all precursor lists when reading spectra headers
		ScanHeaderReader.loadPrecursorList = true
    
		val ms1ShByCycle = mzDbReader.getMs1ScanHeaders().map { sh => sh.getCycle() -> sh } toMap
    val ms2ScanHeaders = mzDbReader.getMs2ScanHeaders().take(1000)
    
    for( sh <- ms2ScanHeaders ) {
      
      val ms1Sh = ms1ShByCycle(sh.getCycle)
      val precMz = sh.getPrecursor.parseFirstSelectedIonMz
      
      val scanSlices = mzDbReader.getScanSlices(precMz - 2, precMz + 2, ms1Sh.getTime - 0.2, ms1Sh.getTime + 0.2, 1)
      if( scanSlices.isEmpty ) {
        logger.debug("no scan data loaded")
      } else {
        require( scanSlices.length == 1, "weird")
        
        val filteredData = scanSlices.head.getData().mzRangeFilter(precMz - 1,  precMz + 1)
        
        if( filteredData != null ) {
          val dataPoints = filteredData.toPeaks(sh).map { peak =>
            Array( (peak.getMz() - 0.00001) -> 0f, peak.getMz() -> peak.getIntensity(), (peak.getMz() + 0.00001) -> 0f)
          } flatten
    
          val chartBytes = createSpectrumChart(dataPoints)
          
          var j = 0
          j+=1; stmt.bind(j, sh.getId() )
          j+=1; stmt.bind(j, sh.getTime() )
          j+=1; stmt.bind(j, precMz )
          j+=1; stmt.bind(j, chartBytes )
          stmt.step()
          stmt.reset()
        }
      }
    }

    connection.exec("COMMIT TRANSACTION")
    connection.dispose()
  }
  
  protected def createSpectrumChart( dataPoints: Array[(Double,Float)]): Array[Byte] = {
    
    val series = new XYSeries("SPECTRUM")
    
    dataPoints.foreach { dataPoint =>
      series.add(dataPoint._1, dataPoint._2 )
    }
    
    val xyDataset = new XYSeriesCollection(series)
    val chart = ChartFactory.createXYLineChart(
      "SPECTRUM", "m/z", "Intensity",
      xyDataset, PlotOrientation.VERTICAL, true, true, false
    )
                
    val bi = chartToImage(chart, 800, 600)
    ChartUtilities.encodeAsPNG( bi )
  }
  
  protected def chartToImage( chart: JFreeChart, width: Int, height: Int): BufferedImage = { 
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g2 = img.createGraphics()
    
    chart.draw(g2, new Rectangle2D.Double(0, 0, width, height))
    g2.dispose()
    
    img
  }
}

object SQLitePeakelStorer {
  
  val sgSmoother = new SavitzkyGolaySmoother( SavitzkyGolaySmoothingConfig())
  val psgSmoother = new PartialSavitzkyGolaySmoother( SavitzkyGolaySmoothingConfig(iterationCount = 1))
  val xicBinner = new XicBinner( XicBinnerConfig(5) )
  
  def storePeakels(peakels: Seq[Peakel], dbLocation: File ) {
    
    println("nb peakels:"+peakels.length)
    
    //val connection = new SQLiteConnection(new File("extracted_xics.sqlite"))
    val connection = new SQLiteConnection(dbLocation)
    connection.open(true) // allowsCreate = true
    
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=8000;")
  
      // Create tables
    connection.exec("CREATE TABLE peakel (" +
      " id INTEGER,\n" +
      " mz REAL,\n" +
      " time REAL,\n" +
      " apex REAL,\n" +
      " ms1_count INTEGER,\n" +
      " factor REAL,\n" +
      " xic BLOB,\n" +
      " smoothed_xic BLOB,\n" +
      " noise_free_xic BLOB,\n" +
      " found_peakels BLOB,\n" +
      " mini_maxi TEXT\n" +
      ")"
    )
    
    connection.exec("BEGIN TRANSACTION")
    
    // Prepare INSERT statement
    val stmt = connection.prepare("INSERT INTO peakel VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",true)
    
    peakels.foreach { peakel =>

      val xic = peakel.elutionTimes.zip(peakel.intensityValues.map(_.toDouble))
      val chartBytes = createXicChart(xic)
      
      /*def calcOscillationCount(xic: Array[(Float,Double)]): Double = {
        val slopes = DerivativeAnalysis.calcTernarySlopes( xic.map(_._2), derivativeLevel = 2)
        
        // Remove isolated osciallations
        var prevSlope = 0.0
        val consecutiveSlopes = for( slope <- slopes ) yield {
          val newSlope = if( prevSlope != 0 && slope != 0) slope else 0
          prevSlope = slope
          newSlope
        }
        
        consecutiveSlopes.map(math.abs(_)).sum
      }
      val oscillationCount = calcOscillationCount(xic)
      val noise = BaseLineRemover.calcNoiseThreshold(xic)*/
      
      // TODO: move to derivative utils ???
      /*def calcMedianSlope(xic: Array[(Float,Double)]): Double = {
        
        val slopes = xic.sliding(2).map { buffer =>
          if(buffer(0)._1 == 0) 0 else math.log(buffer(1)._2 / buffer(0)._2).abs
        } toArray
        
        median(slopes)
      }
      val medianSlope = calcMedianSlope(xic)
      val residualsCV = SignalSmoothingUtils.calcResidualsAbsoluteCV(xic, sgSmoother.smoothTimeIntensityPairs(xic))
      //val residuals = SignalSmoothingUtils.calcSmoothingResiduals(xic, sgSmoother.smoothTimeIntensityPairs(xic))
      //val residualsChartBytes = createXicChart(residuals)
      */
      
      val oscillationFactor = SmartPeakelFinder.calcOscillationFactor(xic)
      
      // Compute histogram of observed intensities
      /*val intensityHistoComputer = new fr.profi.util.stat.EntityHistogramComputer(xic, 
        { rtIntPair: (Float,Double) => rtIntPair._2 }
      )
      val intensityHisto = intensityHistoComputer.calcHistogram(5)
      val xicFreq = intensityHisto.map( bin => bin._1.center.toFloat -> bin._2.length.toDouble )
      val xicFreqChartBytes = createXicChart(xicFreq, scaling = 1)
      */
      
      val smoothedXic = psgSmoother.smoothTimeIntensityPairs(xic)
      val smoothedChartBytes = createXicChart(smoothedXic)
      
      val baselineAnalysis = new BaselineRemover().removeBaseLine(xic)
      val baselineAnalysisChartBytes = createXicChart(baselineAnalysis)
      
      //val binnedXic = xicBinner.binRtIntPairs(xic)
      //val binnedChartBytes = createXicChart(binnedXic)
      
      val peakelsIndices = SmartPeakelFinder.findPeakelsIndices(xic)
      val mergedXic = if( peakelsIndices.isEmpty ) xic
      else {
        val mergedXicBuffer = new ArrayBuffer[(Float,Double)](xic.length)
        for( peakelIndices <- peakelsIndices ) {
          val peaks = xic.slice(peakelIndices._1, peakelIndices._2 + 1)
          mergedXicBuffer += ( (peaks.head._1 - 0.1f) -> 0.0)
          mergedXicBuffer ++= peaks
          mergedXicBuffer += ( (peaks.last._1 + 0.1f) -> 0.0)
        }
        mergedXicBuffer.toArray
      }
      val mergedXicChartBytes = createXicChart(mergedXic)
      //val mergedXicChartBytes: Array[Byte] = null
      
      val miniMaxi = DerivativeAnalysis.findSignificantMiniMaxi(smoothedXic.map(_._2), miniMaxiDistanceThresh = 3, maxIntensityRelThresh = 0.66f)
      
      var j = 0
      j+=1; stmt.bind(j, peakel.getId() )
      j+=1; stmt.bind(j, peakel.getMz() )
      j+=1; stmt.bind(j, peakel.getApexElutionTime() )
      j+=1; stmt.bind(j, peakel.getApexIntensity() )
      j+=1; stmt.bind(j, peakel.getScanIds().length )
      j+=1; stmt.bind(j, oscillationFactor )
      j+=1; stmt.bind(j, chartBytes )
      j+=1; stmt.bind(j, smoothedChartBytes )
      j+=1; stmt.bind(j, baselineAnalysisChartBytes )
      j+=1; stmt.bind(j, mergedXicChartBytes )
      j+=1; stmt.bind(j, miniMaxi.map(m => m.value + s"(${m.isMaximum})").mkString("\t") )
      stmt.step()
      stmt.reset()
      
    }

    connection.exec("COMMIT TRANSACTION")
    connection.dispose()
  }
  
  protected def createXicChart( xic: Array[(Float,Double)], scaling: Float = 1f/60 ): Array[Byte] = {
    
    val series = new XYSeries("XIC")
    
    xic.foreach { dataPoint =>
      series.add(dataPoint._1 * scaling, dataPoint._2 ) // convert seconds into minutes
    }
    
    val xyDataset = new XYSeriesCollection(series)
    val chart = ChartFactory.createXYLineChart(
      "XIC", "Time", "Intensity",
      xyDataset, PlotOrientation.VERTICAL, true, true, false
    )
                
    val bi = chartToImage(chart, 400, 300)
    ChartUtilities.encodeAsPNG( bi )
  }
  
  protected def chartToImage( chart: JFreeChart, width: Int, height: Int): BufferedImage = { 
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g2 = img.createGraphics()
    
    chart.draw(g2, new Rectangle2D.Double(0, 0, width, height))
    g2.dispose()
    
    img
  }
}



/**
 * @author David Bouyssie
 *
 */
object SQLiteFeatureStorer {  
  
  def storeFeatures(features: Seq[Feature], dbLocation: File ) {
    
    val connection = new SQLiteConnection(dbLocation)
    if( dbLocation.exists ) {
      dbLocation.delete()
      //throw new Exception("file already exists")
    }
    
    println("nb features:"+features.length)
    
    // Open SQLite file
    connection.open(true)
    
    // SQLite optimization
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=8000;")

    // Create tables
    connection.exec("CREATE TABLE feature (" +
  	  " id INTEGER,\n" +
  	  " charge INTEGER,\n" +
      " mz REAL,\n" +
      " time REAL,\n" +
      " area REAL,\n" +
      " sum_intensity_top_2 REAL,\n" +
      " ms1_count INTEGER,\n" +
      " isotopes_count INTEGER,\n" +
      " xic_1 BLOB,\n" +
      " xic_2 BLOB,\n" +
      " xic_3 BLOB,\n" +
      " xic_4 BLOB,\n" +
      " xic_5 BLOB,\n" +
      " xic_6 BLOB" +
      ")"
    )
    
    // Begin transaction
    connection.exec("BEGIN TRANSACTION")
    
    // Prepare INSERT statement
    val stmt = connection.prepare("INSERT INTO feature VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",true)

    //var i = 0
    features.sortBy(_.id).foreach { ft => 
      val charts = new Array[Array[Byte]](6)
      for (i <- 0 until 6) {
        if (i < ft.getPeakelsCount) {
          charts(i) = createFeatureChart(ft.getXIC(i))
        } else {
          charts(i) = createFeatureChart(Array[Float](), Array[Float]())
        }
      }
      
      var j = 0
      j+=1; stmt.bind(j, ft.id )
      j+=1; stmt.bind(j, ft.charge )
      j+=1; stmt.bind(j, ft.mz )
      j+=1; stmt.bind(j, ft.getElutionTime )
      j+=1; stmt.bind(j, ft.area )
      j+=1; stmt.bind(j, ft.getPeakel(0).getApexIntensity + ft.getPeakel(1).getApexIntensity)
      j+=1; stmt.bind(j, ft.getMs1Count )
      j+=1; stmt.bind(j, ft.getPeakelsCount )
      j+=1; stmt.bind(j, charts(0))
      j+=1; stmt.bind(j, charts(1))
      j+=1; stmt.bind(j, charts(2))
      j+=1; stmt.bind(j, charts(3))
      j+=1; stmt.bind(j, charts(4))
      j+=1; stmt.bind(j, charts(5))
      stmt.step()
      stmt.reset()
    }
    
    // Commit transaction
    connection.exec("COMMIT TRANSACTION")
    // Close SQLite file
    connection.dispose()
    
  }
  /*
  def storeFeaturesAndComputations(features: Seq[Feature], dbLocation: File ) {
    
    val connection = new SQLiteConnection(dbLocation)
    if( dbLocation.exists ) {
      dbLocation.delete()
      //throw new Exception("file already exists")
    }
    
    // Open SQLite file
    connection.open(true)
    
    // SQLite optimization
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=8000;")

    // Create tables
    connection.exec("CREATE TABLE feature (id INTEGER,\n" +
  		            "                      mz REAL,\n" +
  		            "                      time REAL,\n" +
  		            "                      intensity REAL,\n" +
  		            "                      ms1_count INTEGER,\n" +
  		            "                      peakels_count INTEGER,\n" +
  		            "                      local_maxima_count INTEGER,\n" +
  		            "                      peaks_count INTEGER,\n" +
  		            "                      smoothing_nrsmd REAL,\n" +
  		            "                      xic1 BLOB,\n" +
  		            "                      xic2 BLOB,\n" +
                    "                      xic3 BLOB" +
    		        ")")
    
    // Begin transaction
    connection.exec("BEGIN TRANSACTION")
    
    // Prepare INSERT statement
    val stmt = connection.prepare("INSERT INTO feature VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",true)
    
    features.sortBy(_.id).foreach { ft =>
      
      val xic = ft.getXIC(0)
      if( ft.ms1Count >= 5 ) {
        
        val ftTime = ft.elutionTime/60
      
        val smoothedIntensities = smoothIntensities(xic._2)
        val min = (xic._2 ++ smoothedIntensities).min
        val max = (xic._2 ++ smoothedIntensities).max
        val rmsd = VectorSimilarity.rmsd( xic._2.map(_.toDouble), smoothedIntensities.map(_.toDouble) ).toFloat / (max-min)
        //val smoothedXIC = xicCutoff( (xic._1, smoothedIntensities), 0.2f * max, true )
        val smoothedXIC = (xic._1, smoothedIntensities)
        
        val peaksIndexes = BasicPeakelFinder.findPeakelsIndexes( xic._2.map(_.toDouble), 2 )
        val peaksCount = peaksIndexes.length
        val matchingPeak = peaksIndexes.find( idx => ftTime >= xic._1(idx._1) && ftTime <= xic._1(idx._2) )
        
        val matchingXValues = new ArrayBuffer[Float]
        val matchingYValues = new ArrayBuffer[Float]
        val matchingSmoothedIntensities = new ArrayBuffer[Float]
        if( matchingPeak != None ) {
          for( idx <- matchingPeak.get._1 to matchingPeak.get._2 ) {
            matchingXValues += xic._1(idx)
            matchingYValues += xic._2(idx)
            matchingSmoothedIntensities += smoothedIntensities(idx)
          }
        }
        val adjustedXIC = (matchingXValues.toArray, matchingYValues.toArray)
        val maximaCount = countMaxima( matchingSmoothedIntensities.toArray, 1 )
        
        val xicChart = createFeatureChart(xic._1, xic._2)
        val smoothedXicChart = createFeatureChart( smoothedXIC )
        //val derivativeXIC = calcDerivativeXIC( smoothedXIC )
        //val derivativeXicChart = createFeatureChart( derivativeXIC._1, derivativeXIC._2 )
        //val derivative2XicChart = createFeatureChart( calcDerivativeXIC(derivativeXIC) )
        //val xicCV = calcXicMovingCV( (xic._1, smoothedIntensities) )
        //val xicCVChart = createFeatureChart( xicCV.get )
        //val slopeSigns = accumulateSlopeSigns(smoothedIntensities).map(_.toFloat)
        //val slopeChart = createFeatureChart( (xic._1, slopeSigns) )
        val firstPeakChart= createFeatureChart( matchingXValues.toArray, matchingYValues.toArray )
        
        
        stmt.bind(1, ft.id )
        stmt.bind(2, ft.mz )
        stmt.bind(3, ftTime )
        stmt.bind(4, ft.area )
        stmt.bind(5, matchingXValues.length )
        stmt.bind(6, ft.peakelsCount )
        stmt.bind(7, maximaCount )
        stmt.bind(8, peaksCount )
        stmt.bind(9, rmsd )
        stmt.bind(10, smoothedXicChart )
        stmt.bind(11, xicChart )
        stmt.bind(12, createFeatureChart(ft.getSummedXIC()) )
        stmt.step()
        stmt.reset()
      }
    }
    
    // Commit transaction
    connection.exec("COMMIT TRANSACTION")
    		           
    // Close SQLite file
    connection.dispose()
  }
  
  protected def calcDerivativeXIC( xic: Tuple2[Array[Float],Array[Float]]): Tuple2[Array[Float],Array[Float]] = {
    
    val dxValues = new ArrayBuffer[Float]()
    val dyValues = new ArrayBuffer[Float]()
    if( xic._1.length <= 1 ) return (dxValues.toArray, dyValues.toArray)
    
    xic.zip.sliding(2).foreach { buffer =>
      val aPoint = buffer(0)
      val bPoint = buffer(1)
      
      dyValues += (bPoint._2 - aPoint._2)/(bPoint._1 - aPoint._1)
      dxValues += bPoint._1
    }
    
    (dxValues.toArray, dyValues.toArray)
  }
  
  protected def calcXicMovingCV( xic: Tuple2[Array[Float],Array[Float]]): Option[Tuple2[Array[Float],Array[Float]]] = {
    
    if( xic._1.length <= 3 ) return None
    
    val newXValues = new ArrayBuffer[Float]()
    val newYValues = new ArrayBuffer[Float]()
    newXValues += xic._1.head
    newYValues += 0
    
    xic.zip.sliding(3).foreach { buffer =>
      val yVals = buffer.map(_._2.toDouble)
      val stdDev = math.sqrt( StatUtils.variance(yVals) )
      val mean = StatUtils.mean(yVals)
      val cv = 100 * stdDev / mean
      newXValues += buffer(1)._1
      newYValues += cv.toFloat
    }
    
    newXValues += xic._1.last
    newYValues += 0
    
    Some(newXValues.toArray,newYValues.toArray)
  }
  
  // Basic computation to remove the baseline
  protected def xicCutoff( xic: Tuple2[Array[Float],Array[Float]], threshold: Float, zeroPadding: Boolean = false ): Tuple2[Array[Float],Array[Float]] = {
    
    val newXValues = new ArrayBuffer[Float]()
    val newYValues = new ArrayBuffer[Float]()
    
    xic.zip.foreach { dp =>
      if( dp._2 >= threshold) {
        newXValues += dp._1
        newYValues += dp._2
      }
      else if( zeroPadding ) {
        newXValues += dp._1
        newYValues += 0
      }
    }
    
    (newXValues.toArray,newYValues.toArray)
  }
  
  protected def smoothIntensities(intensities: Array[Float]): Array[Float] = {
    
    val times = 3
    import mr.go.sgfilter.SGFilterMath3
    
    // TODO: static values
    val(nl,nr,order) = (5,5,4)
    val coeffs = SGFilter.computeSGCoefficients(nl,nr,order)

    val sgFilter = new SGFilterMath3(5,5)
    var smoothedIntensities = intensities
    for( i <- 1 to times ) {
      smoothedIntensities = sgFilter.smooth(smoothedIntensities,coeffs)
    }
    
    smoothedIntensities
  }
 
  protected def countMaxima(values: Seq[Float], consNbTimesThresh: Int = 2 ): Int = {
    countSlopes(values, 1, consNbTimesThresh)
  }
  
  protected def countMinima(values: Seq[Float], consNbTimesThresh: Int = 2 ): Int = {
    countSlopes(values, -1, consNbTimesThresh)
  }
  
  protected def countSlopes(values: Seq[Float], slope: Int, consNbTimesThresh: Int ): Int = {
    if( values.length <= 1 ) return 0
      
    var slopesCount = 0
    var nbConsecutiveTimes = 0
    var prevSlope = 0
    
    values.sliding(2).foreach { buffer =>
      val prevValue = buffer(0)
      val curValue = buffer(1)
      val curSlope = (curValue - prevValue).signum
      
      if( curSlope == slope ) {
        nbConsecutiveTimes += 1
      }
      else {
        if( prevSlope == slope && nbConsecutiveTimes >= consNbTimesThresh ) slopesCount += 1
        nbConsecutiveTimes = 0
      }
      
      prevSlope = curSlope
    }
    
    slopesCount
  }
  
  protected def accumulateSlopeSigns(values: Seq[Float] ): Array[Int] = {
    if( values.length <= 1 ) return Array(0)
    
    val slopeSigns = new ArrayBuffer[Int]()
    var prevSlope = 0
    slopeSigns += prevSlope
    
    values.sliding(2).foreach { buffer =>
        
      val prevValue = buffer(0)
      val curValue = buffer(1)
      val slope = (curValue - prevValue).signum
      prevSlope += slope
      
      slopeSigns += prevSlope
    }
    
    slopeSigns.toArray
  }*/
  
  def createFeatureChart( xic: Tuple2[ Array[Float], Array[Float]] ): Array[Byte] = {
    
    val series = new XYSeries("XIC")
    
    xic._1.zip(xic._2).foreach { dataPoint =>
      series.add(dataPoint._1 / 60, dataPoint._2 ) // convert seconds into minutes
    }
    
    val xyDataset = new XYSeriesCollection(series)
    val chart = ChartFactory.createXYLineChart(
                  "Feature XIC", "Time", "Intensity",
                  xyDataset, PlotOrientation.VERTICAL, true, true, false
                )
                
    val bi = chartToImage(chart, 400, 300)
    ChartUtilities.encodeAsPNG( bi )
  }
  
  protected def chartToImage( chart: JFreeChart, width: Int, height: Int):  BufferedImage = { 
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    val g2 = img.createGraphics();
    chart.draw(g2, new Rectangle2D.Double(0, 0, width, height));
    g2.dispose();
    
    img
  }
  
    
}