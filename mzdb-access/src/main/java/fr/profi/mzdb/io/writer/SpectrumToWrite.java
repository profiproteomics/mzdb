package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.SpectrumHeader;
import fr.profi.mzdb.model.SpectrumMetaData;

public class SpectrumToWrite {

   final SpectrumHeader header;
   final SpectrumMetaData metadata;
   final long spectrumId;
   final DataEncoding dataEncoding;

  public SpectrumToWrite(SpectrumHeader sh, SpectrumMetaData metaDataAsText, long spectrumId, DataEncoding dataEnc) {
    this.header = sh;
    this.metadata = metaDataAsText;
    this.spectrumId = spectrumId;
    this.dataEncoding = dataEnc;
  }

}
