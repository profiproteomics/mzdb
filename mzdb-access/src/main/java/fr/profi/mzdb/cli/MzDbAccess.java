package fr.profi.mzdb.cli;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.io.writer.MgfWriter;
import fr.profi.mzdb.io.writer.MgfWriter.PrecursorMassStrategy;
import fr.profi.mzdb.model.*;
import fr.profi.mzdb.MzDbReader;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.Parameter;

/***
 * This class allows to access to a mzDB file and to make some range queries on it. A list of putative
 * features can be provided to extract their corresponding signal.
 * 
 * @version 0.1
 * 
 * @author David Bouyssié
 * 
 */
public class MzDbAccess {

	/**
	 * Print a string in the standard output and terminate the line. Works only if mode<-PRINT_MODE.
	 * 
	 * @param string
	 *            the string to print
	 * @param mode
	 *            can only assume values PRINT_ALWAYS or PRINT_DEBUG.
	 * */
	protected static void println(String string) {
		System.out.println(string);
	}
	
	 /**
   * Print a string in the standard output. Works only if mode<-PRINT_MODE.
   * 
   * @param string
   *            the string to print
   * @param mode
   *            can only assume values PRINT_ALWAYS or PRINT_DEBUG.
   */
  protected static void print(String string) {
    System.out.print(string);
  }
  
	public static class ExtractPeaksCommand {
	  @Parameter
	  private List<String> parameters = new ArrayList<String>();
	  
	  @Parameter(names="-dbFile", description="mzDB file to perform extraction", required=true)
	  private String dbFile = "";
	  
	  @Parameter(names="-minmz", description="minimum m/z value", required=true)
	  private Double minmz = 0.0;
	  
	  @Parameter(names="-maxmz", description="maximum m/z value", required=true)
	  private Double maxmz = 0.0;
	  
	  @Parameter(names="-mintime", description="minimum elution time")
	  private Double mintime = 0.0;
	  
	  @Parameter(names="-maxtime", description="maximum elution time")
	  private Double maxtime = 0.0;
	}
	
	
	public static class CreateMgfCommand {
	  
	  @Parameter
    private List<String> parameters = new ArrayList<String>();
    
    @Parameter(names="-dbFile", description="mzDB file to perform extraction", required=true)
    private String dbFile = "";
    
    @Parameter(names="-output", description="mgf output file path", required=true)
    private String output = "";
    
    @Parameter(names="-precursor-strategy", description="must be on of 'default, nearest, refined'", required=false)
    private PrecursorMassStrategy precStrategy = PrecursorMassStrategy.DEFAULT;
	}
	
	
  private static Peak[] extractPeaks(ExtractPeaksCommand epc) {
      String dbPath = epc.dbFile;
	    double min_mz = epc.minmz;
	    double max_mz = epc.maxmz;
	    double min_time = epc.mintime;
	    double max_time = epc.maxtime;

	    System.out.println("Running mzDBaccess with following parameters :");
	    System.out.println("- min_mz=" + min_mz);
	    System.out.println("- max_mz=" + max_mz);
	    System.out.println("- min_time=" + min_time);
	    System.out.println("- max_time=" + max_time);

	    // String dbPath = "F:/LCMS/huvec/mzdb/OENYD100205_05.raw.mzDB.sqlite";
	    println("accessing to mzDB located at " + dbPath);
	    
	    // Instantiate the mzDB
	    MzDbReader mzDbInstance = null;
	    try {
	      mzDbInstance = new MzDbReader(new File(dbPath), true);
	    } catch (SQLiteException e) {
	      e.printStackTrace();
	    } catch (ClassNotFoundException e) {
	      e.printStackTrace();
	    } catch (FileNotFoundException e) {
	      e.printStackTrace();
	    } 

	    // Retrieve peaks
	    try {
	      Peak[] peaks = mzDbInstance.getPeaks(min_mz, max_mz, min_time, max_time, 1);
	      if (peaks != null) {
	        for (Peak peak : peaks) {
	          println(peak.getMz() + "\t" + peak.getIntensity() + "\t" + peak.getLeftHwhm() + "\t"
	              + peak.getRightHwhm());
	        }
	      }
	      return peaks;
	    } catch (SQLiteException e) {
	      e.printStackTrace();
	    } catch (Exception e) {
	      // TODO Auto-generated catch block
	      e.printStackTrace();
	    }
	    mzDbInstance.close();
	    return null;
	 }
    
    private static void createMgf(CreateMgfCommand cmd) throws SQLiteException, FileNotFoundException {
      String dbFile = cmd.dbFile;
      String output = cmd.output;
      PrecursorMassStrategy pm = cmd.precStrategy;
      MgfWriter writer = new MgfWriter(dbFile);
      writer.write(output, pm);
    }
  
  
   public static void printAvailableCommands(JCommander jc) {
     println("Available commands:");
     for (JCommander e: jc.getCommands().values() ) {
       e.usage();
     }
   }
	 
	 /**
	   * @param args
	   * @throws SQLiteException
	   */
	  public static void main(String[] args) throws SQLiteException {
	    
	    JCommander jc = new JCommander();
	    ExtractPeaksCommand epc = new MzDbAccess.ExtractPeaksCommand();
	    CreateMgfCommand cmgf = new MzDbAccess.CreateMgfCommand();
	    jc.addCommand("extract_peaks", epc);
	    jc.addCommand("create_mgf", cmgf);
	    
	    try {
	      jc.parse(args);
	      
	      String parsedCommand = jc.getParsedCommand();
	      if (parsedCommand == null || parsedCommand == "") {
	        println("No command provided. Exiting");
	        printAvailableCommands(jc);
	        System.exit(1);
	      }
	      if (parsedCommand.equals("extract_peaks")) {
	        extractPeaks(epc);
	      } else if (parsedCommand.equals("create_mgf")) {
	        createMgf(cmgf);
	      }
	    } catch (Exception e) {
	      e.printStackTrace();  
	    }
	  }

}
