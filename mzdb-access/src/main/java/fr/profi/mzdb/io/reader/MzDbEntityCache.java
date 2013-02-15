package fr.profi.mzdb.io.reader;

import java.util.HashMap;

import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.RunSliceHeader;
import fr.profi.mzdb.model.ScanHeader;

/**
 * @author David Bouyssie
 *
 */
public class MzDbEntityCache {
  
  protected ScanHeader[] scanHeaders = null;
  
  protected HashMap<Integer, ScanHeader> scanHeaderById = null;

  protected HashMap<Float, Integer> scanIdByTime = null;
  
  //protected HashMap<Integer, ArrayList<Integer>> _scanIdsByTimeIndex = null;
  
  protected HashMap<Integer, DataEncoding> dataEncodingById = null;
  
  protected HashMap<Integer, DataEncoding> dataEncodingByScanId = null;
  
  protected RunSliceHeader[] runSliceHeaders = null;
  
  protected HashMap<Integer, RunSliceHeader> runSliceHeaderById = null;
  
  public ScanHeader[] getScanHeaders() {
    return scanHeaders;
  }

  public HashMap<Integer, ScanHeader> getScanHeaderById() {
    return scanHeaderById;
  }

  public HashMap<Float, Integer> getScanIdByTime() {
    return scanIdByTime;
  }

  public HashMap<Integer, DataEncoding> getDataEncodingByScanId() {
    return dataEncodingByScanId;
  }

  public RunSliceHeader[] getRunSliceHeaders() {
    return runSliceHeaders;
  }
  
  public HashMap<Integer, RunSliceHeader> getRunSliceHeaderById() {
    return runSliceHeaderById;
  }


}
