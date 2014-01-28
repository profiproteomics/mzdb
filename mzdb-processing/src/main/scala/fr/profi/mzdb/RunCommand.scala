package fr.profi.mzdb

import com.beust.jcommander.{JCommander, MissingCommandException, Parameter, ParameterException, Parameters}
import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable.ArrayBuffer
/**
 * @author David Bouyssie
 *
 */
object RunCommand extends App with Logging {

  // TODO: put in profi-scala-commons (shared with proline admin)
  trait JCommandReflection {
    lazy private val _parametersAnnotation = this.getClass().getAnnotation(classOf[Parameters])

    object Parameters {
      lazy val names = _parametersAnnotation.commandNames()
      lazy val firstName = names(0)
      lazy val description = _parametersAnnotation.commandDescription()
    }
  }
  
  @Parameters(commandNames = Array("xics"), commandDescription = "Generate multiple XICs from a list of peptides", separators = "=")
  private object GenerateXICs extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB file", required = true)
    var mzdbFilePath: String = ""
      
    @Parameter(names = Array("--peplist_file"), description = "The path to the peptide list file", required = true)
    var peplistFilePath: String = ""
      
    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""
     
    @Parameter(names = Array("--mztol_ppm"), description = "The m/z tolerance in PPM for signal extraction", required = true)
    var mzTol: Float = 0f
  }
  
  //ExportRegion
  @Parameters( commandNames = Array("dump_region"), commandDescription = "Extract a Lcms Map region given minmz, maxmz, minrt and maxrt", separators = "=")
  private object DumpRegion extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB file", required = true)
    var mzdbFilePath: String = ""
    
    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""
      
    @Parameter(names = Array("--mzmin"), description = "minimum mz of the requested region", required = true)
    var mzmin:Double = 0d
    
    @Parameter(names = Array("--mzmax"), description = "maximum mz of the requested region", required = true)
    var mzmax: Double = 0d
    
    @Parameter(names = Array("--rtmin"), description = "minimum retention time of the requested region", required = true)
    var rtmin: Float = 0f
    
    @Parameter(names = Array("--rtmax"), description = "maximum retention time of the requested region", required = true)
    var rtmax: Float = 0f

  }
  
  //ExportRegion
  @Parameters( commandNames = Array("dump_region_and_bin"), commandDescription = "Extract a Lcms Map region given minmz, maxmz, minrt and maxrt", separators = "=")
  private object DumpRegionBinning extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB file", required = true)
    var mzdbFilePath: String = ""
    
    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""
      
    @Parameter(names = Array("--nb_bins"), description = "the number of wanted bins", required = true)
    var nbBins: Int = 0
      
    @Parameter(names = Array("--mzmin"), description = "minimum mz of the requested region", required = true)
    var mzmin:Double = 0d
    
    @Parameter(names = Array("--mzmax"), description = "maximum mz of the requested region", required = true)
    var mzmax: Double = 0d
    
    @Parameter(names = Array("--rtmin"), description = "minimum retention time of the requested region", required = true)
    var rtmin: Float = 0f
    
    @Parameter(names = Array("--rtmax"), description = "maximum retention time of the requested region", required = true)
    var rtmax: Float = 0f

  }
  
  override def main(args: Array[String]): Unit = {

    // Instantiate a JCommander object and affect some commands
    val jCmd = new JCommander()
    jCmd.addCommand(GenerateXICs)
    jCmd.addCommand(DumpRegion)
    jCmd.addCommand(DumpRegionBinning)
    // Try to parse the command line
    var parsedCommand = ""
    try {
      jCmd.parse(args: _*)

      parsedCommand = jCmd.getParsedCommand()
      println("Running '" + parsedCommand + "' command...")

      // Execute parsed command
      parsedCommand match {
        case GenerateXICs.Parameters.firstName => {
          val p = GenerateXICs
          generateXICs(p.mzdbFilePath,p.peplistFilePath,p.outputFilePath,p.mzTol)
        }
        case DumpRegion.Parameters.firstName => {
          val p = DumpRegion
          println("" + p.mzmin + ", " +  p.mzmax +  ", "  + p.rtmin + ", " + p.rtmax)
          dumpRegion(p.mzdbFilePath, p.outputFilePath, p.mzmin, p.mzmax, p.rtmin, p.rtmax)
          
        }
        case DumpRegionBinning.Parameters.firstName => {
          val p = DumpRegionBinning
          println("" + p.mzmin + ", " +  p.mzmax +  ", "  + p.rtmin + ", " + p.rtmax)
          dumpRegionBinning(p.mzdbFilePath, p.outputFilePath, p.nbBins, p.mzmin, p.mzmax, p.rtmin, p.rtmax)
        }
      }
    } catch {

      case pEx: ParameterException => {
        println()
        logger.warn("Invalid command or parameter", pEx)
        jCmd.usage()
      }

      case ex: Exception => {
        println()
        logger.error("Execution of command '" + parsedCommand + "' failed", ex)
      }

    }
    
  }
  
  def generateXICs(mzdbFilePath: String, peplistFilePath: String, outputFilePath: String, mzTolInPPM: Float) {
    
    import java.io.File
    import java.io.FileOutputStream
    import java.io.PrintWriter
    import scala.io.Source
    import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
    import fr.profi.mzdb.model.Peak
    import fr.profi.mzdb.model.Peakel
    import fr.profi.mzdb.MzDbReader
    
    case class DetectedPeak(mz: Double, time: Float, duration: Float, area:Float, intensity: Float)
    
    // Define some helper functions
    def peakTime(p: Peak): Float = p.getLcContext().getElutionTime()
    def sumPeaks( peaks: Seq[Peak]): Float = peaks.foldLeft(0f)( (s,p) => s + p.getIntensity() )
    def highestPeakIntensity( peaks: Seq[Peak]): Float = peaks.sortWith( (a,b) => a.getIntensity() > b.getIntensity() ).head.getIntensity()
    
    def extractXICs( mzDb: MzDbReader, mzList: Array[Double] ): Array[Tuple2[Double,Option[DetectedPeak]]] = {
      
      mzList.map { mz =>
        
        val mzTolInDa = mzTolInPPM * mz / 1e6
        val( minMz, maxMz ) = (mz - mzTolInDa, mz + mzTolInDa)
        val peaks = mzDb.getXIC(minMz, maxMz, 1, MzDbReader.XicMethod.MAX )
        val peakelsIndexes = BasicPeakelFinder.findPeakelsIndexes(peaks)
        
        // Retrieve the peakel corresponding to the feature apex
        val peakels = peakelsIndexes.map { peakelIdx =>
          ( peakelIdx._1 to peakelIdx._2 ).toArray.map( peaks(_) )
        }
        
        val sortedPeakels = peakels.sortWith( (a,b) => highestPeakIntensity(a) > highestPeakIntensity(b) )
        
        val intSum = if( sortedPeakels.length > 0 ) {
          
          // Retrieve the apex
          val peaks = sortedPeakels(0)
          
          // Filter the peaks
          val apex = peaks.reduce( (a,b) => if( a.getIntensity() > b.getIntensity() ) a else b )
          val threshold = apex.getIntensity() * 0.05
          val filteredPeaks = peaks.filter( p => p.getIntensity() > threshold )
          
          // Build a peakel
          val peakel = new Peakel(0,filteredPeaks.map(Some(_)))
          
          /*
          // Compute the duration
          val duration = peakTime(filteredPeaks.last) - peakTime(filteredPeaks.head)
          
          apex.getMz(), peakTime(apex), duration, sumPeaks(filteredPeaks),
          */
          
          mz -> Some( DetectedPeak( peakel.mz, peakTime(apex), peakel.duration, peakel.area, apex.getIntensity ) )
        } else mz -> None
        
        intSum
      }

    }
    
    val start = System.currentTimeMillis()
    
    val mzDb = new MzDbReader( mzdbFilePath, true )      
    val mzList = Source.fromFile(peplistFilePath).getLines.map( _.toDouble ).toArray    
    val outStream = new PrintWriter(new FileOutputStream(outputFilePath))
    
    outStream.println( List("input m/z","exp m/z","time","duration","area","intensity").mkString("\t") )
    
    // Output the results
    val peptides = extractXICs( mzDb, mzList )
    for( pep <- peptides ) {
      val mz = pep._1
      if( pep._2.isDefined) {
        val peak = pep._2.get
        val(expMz,time,dur,area,int) = (peak.mz,peak.time/60,peak.duration/60,peak.area,peak.intensity )
        outStream.println( List(mz,expMz,time,dur,area,int).mkString("\t") )
        outStream.flush
        println("found peptide ion of intensity |" + int + "| and duration |"+ dur +"| at |"+time+"|" )
      }
      else println("can't find peptide ion of m/z=" + mz )
    }
    outStream.close()
    mzDb.close()
    
    val took = (System.currentTimeMillis - start)/1000f
    println("extraction took: "+took)
    
    ()
  }
  
  def dumpRegion(mzdbFilePath:String, outputFilePath:String, mzmin:Double, mzmax: Double, rtmin:Float, rtmax:Float) {
    //import java.io.File
    import java.io.FileOutputStream
    import java.io.PrintWriter
    //import scala.io.Source
    import fr.profi.mzdb.MzDbReader
      
    val start = System.currentTimeMillis()
    
    val mzDb = new MzDbReader( mzdbFilePath, true )      
    val outStream = new PrintWriter(new FileOutputStream(outputFilePath))
    val scanSlices = mzDb.getScanSlices(mzmin, mzmax, rtmin, rtmax, 1) // always in mslevel 1 by default
    
    outStream.println( List("moz","time","intensity").mkString("\t") )
    
    scanSlices.foreach( scanSlice => 
      scanSlice.getPeaks().foreach(peak =>
        outStream.println(List(peak.getMz(), peak.getLcContext().getElutionTime(), peak.getIntensity()).mkString("\t"))
      )
    )
    
    outStream.close()
    mzDb.close()
    
    val took = (System.currentTimeMillis - start)/1000f
    println("extraction took: "+took)
  }
  
  def dumpRegionBinning(mzdbFilePath: String, outputFilePath:String, nbBins: Int, mzmin: Double, mzmax: Double, rtmin: Float, rtmax: Float) {
    import java.io.FileOutputStream
    import java.io.PrintWriter
    //import scala.io.Source
    import fr.profi.util.stat.EntityHistogramComputer
    import fr.profi.mzdb.MzDbReader
    import fr.profi.mzdb.model.Peak
      
    val start = System.currentTimeMillis()
    
    val mzDb = new MzDbReader( mzdbFilePath, true )      
    val outStream = new PrintWriter(new FileOutputStream(outputFilePath))
    val scanSlices = mzDb.getScanSlices(mzmin, mzmax, rtmin, rtmax, 1) // always in mslevel 1 by default
    val flattenedPeaksMz =  scanSlices.map(_.getPeaks).flatten.map(_.getMz).sortBy(x=>x)
    val (minmz, maxmz) = (flattenedPeaksMz.head, flattenedPeaksMz.last) 
    

    val binner = new EntityHistogramComputer(flattenedPeaksMz, (x:Double)=> x)
    val mzBins = binner.calcHistogram(nbBins)
    
    val mzList = mzBins.map{ case (bin, values) => bin.center } toArray
    val rtList = scanSlices.map(_.getHeader().getElutionTime()) toArray
    val intList = Array.ofDim[Float](scanSlices.length, mzBins.length)
    
    var i = 0
    scanSlices.foreach{ scanSlice =>
      val binner_ = new EntityHistogramComputer(scanSlice.getPeaks, (x : Peak) => x.getMz())
      val bins = binner_.calcHistogram(nbBins, range=Some( Pair(minmz, maxmz) ) )
      var j = 0
      bins.foreach{ case (bin, values) =>
        intList(i)(j) = values.map(_.getIntensity).sum
        j+=1
      }
      i+=1
    }
    outStream.println("moz: " + mzList.mkString("\t"))
    outStream.println("rt: " + rtList.mkString("\t"))
    val intString = intList.map(_.mkString("\t")).toArray.mkString(",")
    outStream.println("intensities: "+  intList.map(_.mkString("\t")).toArray.mkString(",") )
    
    outStream.close()
    mzDb.close()
    
    val took = (System.currentTimeMillis - start)/1000f
    println("extraction took: "+took)
  }

  
}