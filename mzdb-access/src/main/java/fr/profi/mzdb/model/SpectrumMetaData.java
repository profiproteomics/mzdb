package fr.profi.mzdb.model;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

/**
 * Contains only few properties of Spectrum, in String format
 *  spectrumId: Long,  =SpectrumHeader.id
 *  paramTree: String, =SpectrumHeader.paramTree
 *  scanList: String,  = SpectrumHeader.scanList
 *  precursorList: Option[String], = SpectrumHeader.precursor
 *
 */
public class SpectrumMetaData implements SerializationInterface {

  protected Long spectrumId;
  protected String paramTree;
  protected String scanList;
  protected String precursorList;

  public SpectrumMetaData(SerializationReader reader) throws IOException {
    read(reader);
  }


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

  @Override
  public void write(SerializationWriter writer) throws IOException {
    boolean hasData = spectrumId!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeInt64(spectrumId);
    }

    hasData = paramTree!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(paramTree);
    }

    hasData = scanList!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(scanList);
    }

    hasData = precursorList!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(precursorList);
    }
  }


  @Override
  public void read(SerializationReader reader) throws IOException {
    boolean hasData = reader.readBoolean();
    if (hasData) {
      spectrumId = reader.readInt64();
    } else {
      spectrumId = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      paramTree = reader.readString();
    } else {
      paramTree = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      scanList = reader.readString();
    } else {
      scanList = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      precursorList = reader.readString();
    } else {
      precursorList = null;
    }
  }
}
