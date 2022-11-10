package fr.profi.mzdb.cli

import com.beust.jcommander._
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.ThreadLogger

/**
 * @auhor David Bouyssie
 * @author CB205360
 */
//warning: overriding method main in trait App is deprecated (since 2.11.0): main should not be overridden
object MzDbProcessing extends App with LazyLogging {
  
  // TODO: put in profi-scala-commons (shared with proline admin)
  trait JCommandReflection {
    lazy private val _parametersAnnotation = this.getClass().getAnnotation(classOf[Parameters])

    object Parameters {
      lazy val names = _parametersAnnotation.commandNames()
      lazy val firstName = names.head
      lazy val description = _parametersAnnotation.commandDescription()
    }
  }

  /*
   * XIC EXTRACTION
   */
  @Parameters(commandNames = Array("xics"), commandDescription = "Generate multiple XICs from a list of peptides", separators = "=")
  private[cli] object GenerateXICs extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB list file", required = true)
    var mzdbFilePath: String = ""

    @Parameter(names = Array("--peplist_file"), description = "The path to the peptide list file", required = true)
    var peplistFilePath: String = ""

    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""

    @Parameter(names = Array("--mztol_ppm"), description = "The m/z tolerance in PPM for signal extraction", required = true)
    var mzTol: Float = 0f

    @Parameter(names = Array("--algo"), description = "The algorithm used to detect XIC: 'basic' or 'wavelet' ", required = false)
    var algo: String = "basic"
  }

  /*
   * extract putative feature using predicted time algorithm
   */
  @Parameters(commandNames = Array("extract_predicted_features"), commandDescription = "Use predicted time extractor to extract provided putative features", separators = "=")
  private[cli] object ExtractPutativeFts extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB list file", required = true)
    var mzdbFilePath: String = ""

    @Parameter(names = Array("--pft_file"), description = "The path to the putative features list file", required = true)
    var pftsFile: String = ""

    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = false)
    var outputFilePath: String = ""

    @Parameter(names = Array("--mztol_ppm"), description = "The m/z tolerance in PPM for signal extraction", required = false)
    var mzTol: Float = 10f

    @Parameter(names = Array("--algo"), description = "The algorithm used to detect XIC: 'basic' or 'wavelet' ", required = false)
    var algo: String = "basic"
  }

  /*
   * ExportRegion
   */
  @Parameters(commandNames = Array("dump_region"), commandDescription = "Extract a Lcms Map region given minmz, maxmz, minrt and maxrt", separators = "=")
  private[cli] object DumpRegion extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB file", required = true)
    var mzdbFilePath: String = ""

    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""

    @Parameter(names = Array("--mzmin"), description = "minimum mz of the requested region", required = true)
    var mzmin: Double = 0d

    @Parameter(names = Array("--mzmax"), description = "maximum mz of the requested region", required = true)
    var mzmax: Double = 0d

    @Parameter(names = Array("--rtmin"), description = "minimum retention time of the requested region", required = true)
    var rtmin: Float = 0f

    @Parameter(names = Array("--rtmax"), description = "maximum retention time of the requested region", required = true)
    var rtmax: Float = 0f
  }

  /*
   * ExportRegionAndBin
   */
  @Parameters(commandNames = Array("dump_region_and_bin"), commandDescription = "Extract a Lcms Map region given minmz, maxmz, minrt and maxrt", separators = "=")
  private[cli] object DumpRegionBinning extends JCommandReflection {
    @Parameter(names = Array("--mzdb_file"), description = "The path to the mzDB file", required = true)
    var mzdbFilePath: String = ""

    @Parameter(names = Array("--output_file"), description = "The path to the output file", required = true)
    var outputFilePath: String = ""

    @Parameter(names = Array("--nb_bins"), description = "the number of wanted bins", required = true)
    var nbBins: Int = 0

    @Parameter(names = Array("--mzmin"), description = "minimum mz of the requested region", required = true)
    var mzmin: Double = 0d

    @Parameter(names = Array("--mzmax"), description = "maximum mz of the requested region", required = true)
    var mzmax: Double = 0d

    @Parameter(names = Array("--rtmin"), description = "minimum retention time of the requested region", required = true)
    var rtmin: Float = 0f

    @Parameter(names = Array("--rtmax"), description = "maximum retention time of the requested region", required = true)
    var rtmax: Float = 0f
  }

  /*
   * Create MGF file
   */
  @Parameters(commandNames = Array("create_mgf"), commandDescription = "Create an MGF peaklist from a mzdb file", separators = "=")
  private[cli] object CreateMgfCommand extends JCommandReflection {

    @Parameter(names = Array("-mzdb", "--mzdb_file_path"), description = "mzDB file to perform extraction", required = true)
    var mzdbFile: String = ""

    @Parameter(names = Array("-o", "--output_file_path"), description = "mgf output file path", required = true)
    var outputFile: String = ""

    @Parameter(names = Array("-ms", "--ms_level"), description = "the MS level to export", required = false)
    var msLevel: Int = 2

    @Parameter(names = Array("-precmz", "--precursor_mz"), description = "must be on of 'main_precursor_mz, selected_ion_mz, refined, refined_thermo, isolation_window_extracted, isolation_window_extracted_v3'", required = false)
    var precMzComputation: String = "main_precursor_mz"

    @Parameter(names = Array("-mztol", "--mz_tol_ppm"), description = "m/z tolerance used for precursor m/z value definition", required = false)
    var mzTolPPM: Float = 20

    @Parameter(names = Array("-cutoff", "--intensity_cutoff"), description = "optional intensity cutoff to use", required = false)
    var intensityCutoff: Float = 0f

    @Parameter(names = Array("-ptitle", "--proline_title"), description = "export TITLE using the Proline convention", required = false)
    var exportProlineTitle: Boolean = false

  }

  /*
  * Dump scan headers
  */
  @Parameters(commandNames = Array("dump_scan_headers"), commandDescription = "Dump headers", separators = "=")
  private[cli] object DumpScanHeaders extends JCommandReflection {

    @Parameter(names = Array("-mzdb", "--mzdb_file_path"), description = "mzDB file to perform extraction", required = true)
    var mzdbFilePath: String = ""

    @Parameter(names = Array("-o", "--output_file_path"), description = "mgf output file path", required = true)
    var outputFilePath: String = ""

    @Parameter(names = Array("-ms", "--ms_level"), description = "the MS level to export", required = false)
    var msLevel: Int = 2
  }

     Thread.currentThread.setUncaughtExceptionHandler(new ThreadLogger(logger.underlying.getName()))
    
    // Instantiate a JCommander object and set some commands
    val jCmd = new JCommander()
    jCmd.addCommand(GenerateXICs)
    jCmd.addCommand(ExtractPutativeFts)
    jCmd.addCommand(DumpRegion)
    jCmd.addCommand(DumpRegionBinning)
    jCmd.addCommand(DumpScanHeaders)
    jCmd.addCommand(CreateMgfCommand)

    // Try to parse the command line
    var parsedCommand = ""
    try {
      jCmd.parse(args: _*)
      parsedCommand = jCmd.getParsedCommand()
      
      if( parsedCommand == null || parsedCommand.isEmpty() ) {
        println("A command must be provided !\n")
        jCmd.usage()
        System.exit(1)
      } else {
        this.logger.info(s"Running '$parsedCommand' command...")
      }
      
      import Commands._
      
      // Execute parsed command
      parsedCommand match {
        case GenerateXICs.Parameters.firstName => {
          val p = GenerateXICs
          generateXICs(p.mzdbFilePath, p.peplistFilePath, p.outputFilePath, p.mzTol, p.algo)
        }
        case ExtractPutativeFts.Parameters.firstName => {
          val p = ExtractPutativeFts
          extractFeatures(p.mzdbFilePath, p.pftsFile, p.outputFilePath)
        }
        case DumpRegion.Parameters.firstName => {
          val p = DumpRegion
          this.logger.info("" + p.mzmin + ", " + p.mzmax + ", " + p.rtmin + ", " + p.rtmax)
          dumpRegion(p.mzdbFilePath, p.outputFilePath, p.mzmin, p.mzmax, p.rtmin, p.rtmax)

        }
        case DumpRegionBinning.Parameters.firstName => {
          val p = DumpRegionBinning
          this.logger.info("" + p.mzmin + ", " + p.mzmax + ", " + p.rtmin + ", " + p.rtmax)
          dumpRegionBinning(p.mzdbFilePath, p.outputFilePath, p.nbBins, p.mzmin, p.mzmax, p.rtmin, p.rtmax)
        }
        case DumpScanHeaders.Parameters.firstName => {
          val p = DumpScanHeaders
          dumpScanHeaders(p.mzdbFilePath, p.outputFilePath, p.msLevel)
        }
        case CreateMgfCommand.Parameters.firstName => {
          logger.info("Creating MGF file ...");
          createMgf()
        }
        case _ => {
          throw new MissingCommandException(s"Unknown command '${jCmd.getParsedCommand}'")
        }
      }
      
    } catch {

      case pEx: ParameterException => {
        println()
        logger.warn("Invalid command or parameter", pEx)
        jCmd.usage()
        System.exit(1)
      }

      case ex: Exception => {
        println()
        logger.error(s"Execution of command '$parsedCommand' failed", ex)
        System.exit(1)
      }
    }

}