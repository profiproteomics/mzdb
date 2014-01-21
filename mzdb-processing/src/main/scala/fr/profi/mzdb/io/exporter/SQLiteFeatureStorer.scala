package fr.profi.mzdb.io.exporter

import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import org.jfree.chart._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.general.DefaultPieDataset
import org.jfree.data.xy._
import org.jfree.data._
import org.apache.commons.math.stat.StatUtils
import com.almworks.sqlite4java.SQLiteConnection
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder


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
      " mz REAL,\n" +
      " time REAL,\n" +
      " intensity REAL,\n" +
      " ms1_count INTEGER,\n" +
      " isotopes_count INTEGER,\n" +
      " xic1 BLOB,\n" +
      " xic2 BLOB,\n" +
      " xic3 BLOB" +
      ")"
    )
    
    // Begin transaction
    connection.exec("BEGIN TRANSACTION")
    
    // Prepare INSERT statement
    val stmt = connection.prepare("INSERT INTO feature VALUES (?,?,?,?,?,?,?,?,?)",true)
    
    var i = 0
    features.sortBy(_.id).grouped(1000).foreach { ftBuffer =>      
      
      if( i < 1000 ) {
        // Compute XICs in parallel
        val xicsBuffer = ftBuffer.par.map { ft => 
          val xic = ft.getXIC(0)
          Pair(createFeatureChart(xic._1, xic._2),createFeatureChart(ft.getSummedXIC()))
        }.toArray

        ftBuffer.zip(xicsBuffer).foreach { case (ft,xics) =>
          i += 1
          
          stmt.bind(1, ft.id )
          stmt.bind(2, ft.mz )
          stmt.bind(3, ft.elutionTime/60 )
          stmt.bind(4, ft.area )
          stmt.bind(5, ft.ms1Count )
          stmt.bind(6, ft.peakelsCount )
          stmt.bind(7, xics._1 )
          stmt.bind(8, xics._2 )
          stmt.bind(9, Array.empty[Byte] )
          stmt.step()
          stmt.reset()
        }
      }

    }
    
    // Commit transaction
    connection.exec("COMMIT TRANSACTION")
                       
    // Close SQLite file
    connection.dispose()
    
  }

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
    
    xic._1.zip(xic._2).sliding(2).foreach { buffer =>
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
    
    xic._1.zip(xic._2).sliding(3).foreach { buffer =>
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
    
    xic._1.zip(xic._2).foreach { dp =>
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
    import mr.go.sgfilter.SGFilter
    
    // TODO: static values
    val(nl,nr,order) = (5,5,4)
    val coeffs = SGFilter.computeSGCoefficients(nl,nr,order)

    val sgFilter = new SGFilter(5,5)
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
  }
  
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