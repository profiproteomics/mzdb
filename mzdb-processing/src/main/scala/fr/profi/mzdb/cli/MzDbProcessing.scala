package fr.profi.mzdb.cli

import com.typesafe.scalalogging.LazyLogging
import com.beust.jcommander.Parameter
import com.beust.jcommander.Parameters
import fr.profi.mzdb.io.writer.mgf.PrecursorMzComputationEnum
import com.beust.jcommander.JCommander
import fr.profi.util.ThreadLogger
import fr.profi.mzdb.io.writer.mgf.MgfWriter
import com.beust.jcommander.ParameterException
import fr.profi.mzdb.io.writer.mgf.IsolationWindowPrecursorExtractor

/**
 * @author CB205360
 */
object MzDbProcessing extends App with LazyLogging {

  @Parameters(commandNames = Array("create_mgf"), commandDescription = "Create an MGF peaklist from a mzdb file", separators = "=")
  private object CreateMgfCommand {

    @Parameter(names = Array("-mzdb", "--mzdb_file_path"), description = "mzDB file to perform extraction", required = true)
    var mzdbFile: String = ""

    @Parameter(names = Array("-o", "--output_file_path"), description = "mgf output file path", required = true)
    var outputFile: String = ""

    @Parameter(names = Array("-precmz", "--precursor_mz"), description = "must be on of 'main_precursor_mz, selected_ion_mz, refined, refined_thermo, isolation_window_extracted'", required = false)
    var precMzComputation: String = "main_precursor_mz"

    @Parameter(names = Array("-mztol", "--mz_tol_ppm"), description = "m/z tolerance used for precursor m/z value definition", required = false)
    var mzTolPPM: Float = 20

    @Parameter(names = Array("-cutoff", "--intensity_cutoff"), description = "optional intensity cutoff to use", required = false)
    var intensityCutoff: Float = 0f

    @Parameter(names = Array("-ptitle", "--proline_title"), description = "export TITLE using the Proline convention", required = false)
    var exportProlineTitle: Boolean = false
  }

  override def main(args: Array[String]): Unit = {
    Thread.currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName()))
    val jCmd = new JCommander()
    jCmd.addCommand(CreateMgfCommand)

    var parsedCommand = ""

    try {
      jCmd.parse(args: _*)
      parsedCommand = jCmd.getParsedCommand()
      
      if( parsedCommand == null || parsedCommand.isEmpty() ) {
        println("A command must be provided !\n")
        jCmd.usage()
        System.exit(1)
      }
      
      if (parsedCommand != null && parsedCommand.equals("create_mgf")) {
        createMgf()
      }
      
    } catch {

      case pEx: ParameterException => {
        logger.warn("Invalid command or parameter", pEx)
        jCmd.usage()
      }

      case ex: Exception => {
        logger.error("Execution of command '" + parsedCommand + "' failed", ex)
        jCmd.usage()
      }

    }
  }

  def createMgf(): Unit = {

    logger.info("Creating MGF File for mzDB at: " + CreateMgfCommand.mzdbFile);
    logger.info("Precursor m/z values will be defined using the method: " + CreateMgfCommand.precMzComputation);

    val writer = new MgfWriter(CreateMgfCommand.mzdbFile);
    val precCompEnum = PrecursorMzComputationEnum.values().find(_.name() == CreateMgfCommand.precMzComputation)
    
    if (precCompEnum.isDefined) { 
       writer.write(CreateMgfCommand.outputFile,  precCompEnum.get, CreateMgfCommand.mzTolPPM, CreateMgfCommand.intensityCutoff, CreateMgfCommand.exportProlineTitle);
    } else if (CreateMgfCommand.precMzComputation == "isolation_window_extracted") {
       val precComputer = new IsolationWindowPrecursorExtractor(CreateMgfCommand.mzTolPPM)
       writer.write(CreateMgfCommand.outputFile, precComputer, CreateMgfCommand.intensityCutoff, CreateMgfCommand.exportProlineTitle);
    } else {
      throw new IllegalArgumentException
    }
  }
}