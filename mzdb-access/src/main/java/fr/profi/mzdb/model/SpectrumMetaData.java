package fr.profi.mzdb.model;

/**
 * Contains only few properties of Spectrum, in String format
 *  spectrumId: Long,  =SpectrumHeader.id
 *  paramTree: String, =SpectrumHeader.paramTree
 *  scanList: String,  = SpectrumHeader.scanList
 *  precursorList: Option[String], = SpectrumHeader.precursor
 *
 */
public class SpectrumMetaData {

  protected final Long spectrumId;
  protected final String paramTree;
  protected final String scanList;
  protected final String precursorList;


  public SpectrumMetaData(Long spectrumId, String paramTree, String scanList, String precursorList) {
    this.spectrumId = spectrumId;
    this.paramTree = paramTree;
    this.scanList = scanList;
    this.precursorList = precursorList;
  }

  public Long getSpectrumId() {
    return spectrumId;
  }

  public String getParamTree() {
    return paramTree;
  }

  public String getScanList() {
    return scanList;
  }

  public String getPrecursorList() {
    return precursorList;
  }
}
