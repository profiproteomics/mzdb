package fr.profi.mzdb

import com.beust.jcommander.{JCommander, MissingCommandException, Parameter, ParameterException, Parameters}
import com.weiglewilczek.slf4s.Logging
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
  
  override def main(args: Array[String]): Unit = {

    // Instantiate a JCommander object and affect some commands
    val jCmd = new JCommander()
    jCmd.addCommand(GenerateXICs)

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
    
    val took = (System.currentTimeMillis - start)/1000
    println("extraction took: "+took)
    
    ()
  }
  
}