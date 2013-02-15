package fr.profi.mzdb.io.reader;

import java.util.Iterator;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.RunSlice;
import fr.profi.mzdb.model.RunSliceData;

import com.almworks.sqlite4java.SQLiteException;

// TODO: Auto-generated Javadoc
/**
 * The Class RunSliceDataProvider.
 *
 * @author David Bouyssie
 */
public class RunSliceDataProvider {

  /** The mz db instance. */
  MzDbReader mzDBInstance;
  
  /** The rsd iter. */
  Iterator<RunSlice> rsdIter;
  
  /**
   * Instantiates a new run slice data provider.
   *
   * @param mzDBInstance the mz db instance
   */
  public RunSliceDataProvider(MzDbReader mzDBInstance) {
    super();
    this.mzDBInstance = mzDBInstance;
  }
  
  
  /**
   * Instantiates a new run slice data provider.
   *
   * @param rsdIter the rsd iter
   */
  public RunSliceDataProvider( Iterator<RunSlice> rsdIter) {
    super();
    this.rsdIter = rsdIter;
  }


  /**
   * Gets the run slice data.
   *
   * @param runSliceId the run slice id
   * @return the run slice data
   * @throws SQLiteException the sQ lite exception
   */
  public RunSliceData getRunSliceData( int runSliceId ) throws SQLiteException {
    
    if( rsdIter != null ) {
      return _getNextMatchingRunSliceData(runSliceId);
    } 
    else {
      return mzDBInstance.getRunSliceData(runSliceId);   
    }
  }
  
  /**
   * _get next matching run slice data.
   *
   * @param runSliceNumber the run slice number
   * @return the run slice data
   * @throws SQLiteException the sQ lite exception
   */
  private RunSliceData _getNextMatchingRunSliceData(int runSliceNumber) throws SQLiteException {
    
    // Iterate over run slices to retrieve the wanted run slice
    /*
	Iterator<RunSlice> iter = mzDBInstance.getRunSliceDataIterator();
    RunSlice tmpRs = iter.next();
	while( iter.hasNext() && tmpRs.getHeader().number != runSliceNumber) {
      tmpRs = iter.next();
	}
	return tmpRs.getData();
  }*/
	  while (rsdIter.hasNext()) {
		  RunSlice tmpRs = rsdIter.next();
		  //return tmpRs.getData();
		  
		  if( tmpRs.getHeader().getNumber() == runSliceNumber )
			  return tmpRs.getData();
	      else if( tmpRs.getHeader().getNumber() > runSliceNumber )
	    	  return null;
	  }
	  return null;
  }
      //return tmpRs.getData();
      //if( tmpRs.getHeader().number == runSliceNumber )
       // return tmpRs.getData();
      //else if( tmpRs.getHeader().number > runSliceNumber )
    	//  return null;
} 
    
    //return null;
      
  

