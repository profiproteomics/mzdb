package fr.profi.mzdb

import com.beust.jcommander.{JCommander, MissingCommandException, Parameter, ParameterException, Parameters}
import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.algo.signal.detection.WaveletPeakelFinder
import fr.profi.mzdb.algo.signal.detection.waveletImpl.WaveletDetectorDuMethod

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
  
  /*
   * XIC EXTRACTION
   */
  @Parameters(commandNames = Array("xics"), commandDescription = "Generate multiple XICs from a list of peptides", separators = "=")
  private object GenerateXICs extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB list file", required = true)
    var mzdbFilePath: String = ""
      
    @Parameter(names = Array("--area"), description = "The kind of area to compute: 'uahm' or 'total', default:'total'", required = false)
    var area: String = "uahm"
      
    @Parameter(names = Array("--peplist_file"), description = "The path to the peptide list file", required = true)
    var peplistFilePath: String = ""
      
    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""
     
    @Parameter(names = Array("--mztol_ppm"), description = "The m/z tolerance in PPM for signal extraction", required = true)
    var mzTol: Float = 0f
    
    @Parameter(names = Array("--algo"), description="The algorithm used to detect XIC: 'basic' or 'wavelet' ", required = false)
    var algo: String = "basic"
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
      this.logger.info("Running '" + parsedCommand + "' command...")

      // Execute parsed command
      parsedCommand match {
        case GenerateXICs.Parameters.firstName => {
          val p = GenerateXICs
          generateXICs(p.mzdbFilePath,p.peplistFilePath,p.outputFilePath, p.area, p.mzTol,p.algo)
        }
        case DumpRegion.Parameters.firstName => {
          val p = DumpRegion
          this.logger.info("" + p.mzmin + ", " +  p.mzmax +  ", "  + p.rtmin + ", " + p.rtmax)
          dumpRegion(p.mzdbFilePath, p.outputFilePath, p.mzmin, p.mzmax, p.rtmin, p.rtmax)
          
        }
        case DumpRegionBinning.Parameters.firstName => {
          val p = DumpRegionBinning
          this.logger.info("" + p.mzmin + ", " +  p.mzmax +  ", "  + p.rtmin + ", " + p.rtmax)
          dumpRegionBinning(p.mzdbFilePath, p.outputFilePath, p.nbBins, p.mzmin, p.mzmax, p.rtmin, p.rtmax)
        }
        case _ => {
          throw new MissingCommandException("unknown command '" + jCmd.getParsedCommand() + "'")
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
  
  def generateXICs(mzdblistFilePath: String, peplistFilePath: String, outputFilePath: String, area: String, mzTolInPPM: Float, algo:String) {
    
    import java.io.File
    import java.io.FileOutputStream
    import java.io.PrintWriter
    import scala.io.Source
    import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
    import fr.profi.mzdb.model.Peak
    import fr.profi.mzdb.model.Peakel
    import fr.profi.mzdb.MzDbReader
    
    case class DetectedPeak(mz: Double, apex: Peak, duration: Float, area: Float ) {
      lazy val intensity: Float = apex.getIntensity()
      lazy val time: Float = apex.getLcContext().getElutionTime()
      
      def getApexFullScanIntensity( mzDb: MzDbReader ): Float = {
        val scan = mzDb.getScan(apex.getLcContext().getScanId())
        scan.getHeader().getTIC()
      }
      
    }
    
     def getArea(p:Peakel):Float = {if (area == "uahm") p.uahm else p.area } 
    // Define some helper functions
    //def peakTime(p: Peak): Float = p.getLcContext().getElutionTime()
    def sumPeaks( peaks: Seq[Peak]): Float = peaks.foldLeft(0f)( (s,p) => s + p.getIntensity() )
    def highestPeakIntensity( peaks: Seq[Peak]): Float = peaks.sortWith( (a,b) => a.getIntensity() > b.getIntensity() ).head.getIntensity()
    
    def getMzdbList() : Array[String] = {
       val a = Source.fromFile(mzdblistFilePath).getLines.toArray
       this.logger.info( s"Found #${a.length} files to analyse" )
       a
    }
    
    
    def getPeptideList ( pepFile: java.io.File): Array[Tuple3[Double, Float, Float]] = {
      val mzList = Source.fromFile(pepFile.getAbsolutePath()).getLines.map{l => 
        val splitted = "\\s".r.split(l)
        var returnVal : Tuple3[Double, Float, Float] = null;
        
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
    
    
    def extractXICs( mzDb: MzDbReader, mzList: Array[Tuple3[Double, Float, Float]] ): Array[Tuple2[Double,Option[DetectedPeak]]] = {
      
      // extract xics NOT in parallel sqlite4java fails
      val peakMatrix = mzList.map { case (mz, rtmin, rtmax)  => 
        val mzTolInDa = mzTolInPPM * mz / 1e6
        val( minMz, maxMz ) = (mz - mzTolInDa, mz + mzTolInDa)
        val peaks = mzDb.getXIC(minMz, maxMz, 1, MzDbReader.XicMethod.MAX )
        this.logger.info(s"XIC for mass ${mz} contains #${peaks.length} peaks")                                     
        (mz, peaks, (rtmin, rtmax))
      }
      
      if (algo == "basic") this.logger.info("basic algorithm")
      else if (algo == "wavelet") this.logger.info("wavelet algorithm")
      else throw new Exception("Specified algorithm is not defined:" + algo)


      // peaks detection on xics on parallel
      peakMatrix.par.map { case (mz, peaks, (rtmin, rtmax)) =>
        var peakelIndexes : Array[(Int, Int)] = null
        if (algo == "basic") {
          peakelIndexes = BasicPeakelFinder.findPeakelsIndexes(peaks)
        } else if (algo == "wavelet") {
          val wpf = new WaveletDetectorDuMethod(peaks)
          wpf.ridgeFilteringParams.minSNR = 0.0f
          peakelIndexes = wpf.findPeakelsIndexes(asScanId= false)
        }
        // Retrieve the peakel corresponding to the feature apex
        val peakels = if (rtmin == 0f && rtmax == 0f) 
                        peakelIndexes.map { peakelIdx => ( peakelIdx._1 to peakelIdx._2 ).toArray.map( peaks(_) ) }
                      else 
                        peakelIndexes.withFilter(x=> peaks(x._1).getLcContext().getElutionTime()/60.0 > rtmin && 
                                                     peaks(x._2).getLcContext().getElutionTime()/60.0 < rtmax )
                                     .map{ peakelIdx => ( peakelIdx._1 to peakelIdx._2 ).toArray.map( peaks(_) ) }
        
        val sortedPeakels = peakels.sortWith( (a,b) => highestPeakIntensity(a) > highestPeakIntensity(b) )
        
        val intSum = if( sortedPeakels.length > 0 ) {
          
          // Retrieve the apex
          val peaks = sortedPeakels(0)
          
          // Filter the peaks
          val apex = peaks.reduce( (a,b) => if( a.getIntensity() > b.getIntensity() ) a else b )
          val threshold = apex.getIntensity() * 0.05
          val filteredPeaks = peaks.filter( p => p.getIntensity() > threshold )
          
          // Build a peakel
          val peakel = new Peakel(0,filteredPeaks)//.map(Some(_)))
          /*
          // Compute the duration
          val duration = peakTime(filteredPeaks.last) - peakTime(filteredPeaks.head)
          
          apex.getMz(), peakTime(apex), duration, sumPeaks(filteredPeaks),
          */
          mz -> Some( DetectedPeak( peakel.mz, apex, peakel.duration, getArea(peakel)) )
        } else mz -> None
        
        intSum
      } toArray
    
    }//ends function
    
    def extractXICsForOneFile(mzdbFilePath: String, mzList: Array[(Double, Float, Float)]) : Array[String] = {
      val start = System.currentTimeMillis()
      
      var mzDb: MzDbReader = null
      try {
       val mzDb = new MzDbReader( mzdbFilePath, true )
        val xicsBuilder = new ArrayBuffer[String]
  
        // Output the results
        val peptides = extractXICs( mzDb, mzList )
        for( pep <- peptides ) {
          val mz = pep._1
          if( pep._2.isDefined) {
            val peak = pep._2.get
            val(expMz,time,dur,area,int,msScanInt) = (peak.mz,peak.time/60,peak.duration/60,peak.area,peak.intensity, peak.getApexFullScanIntensity(mzDb) )
            xicsBuilder +=  List(mzdbFilePath, mz,expMz,time,dur,area,int,msScanInt).mkString("\t") 
            //writer.println(List(mzdbFilePath, mz,expMz,time,dur,area,int,msScanInt).mkString("\t"))
            println("found peptide ion of intensity |" + int + "| and duration |"+ dur +"| at |"+time+"|" )
          }
          else println("can't find peptide ion of m/z=" + mz )
        }
       
        
        val took = (System.currentTimeMillis - start)/1000f
        this.logger.info("extraction took: "+took)
        xicsBuilder.toArray
        
      } catch {
        case e: Exception => {
          this.logger.error("Extraction failed for file " + mzdbFilePath + " because: " + e.getMessage() )
          //e.printStackTrace()
          Array.empty[String]
        }
      } finally {
         if (mzDb != null) 
           mzDb.close()
      }

    }//end function
    
    val mzList = getPeptideList(new java.io.File(peplistFilePath))

    val writer = new PrintWriter(new FileOutputStream(outputFilePath))
    writer.println( List("file name","input m/z","exp m/z","time","duration","area","intensity","ms1_intensity").mkString("\t") )
    writer.flush()
    
    this.logger.info("Starting the extraction of XICs..." )
    val xics = getMzdbList().map(extractXICsForOneFile(_, mzList ))
    xics.flatMap(x=>x).foreach(writer.println(_))
    writer.close()
    
    this.logger.info("Extraction of XICs finished !" )
  }
  
  /**
   * 
   */
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