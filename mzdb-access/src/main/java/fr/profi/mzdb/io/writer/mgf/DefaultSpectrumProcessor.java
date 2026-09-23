package fr.profi.mzdb.io.writer.mgf;

import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.DataMode;
import fr.profi.mzdb.model.SpectrumData;
import fr.profi.mzdb.model.SpectrumHeader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DefaultSpectrumProcessor implements ISpectrumProcessor {


  final Logger logger = LoggerFactory.getLogger(DefaultSpectrumProcessor.class);

  @Override
  public boolean accept(MzDbReader mzDbReader) {
    try {
      SpectrumHeader firstMS2Spectrum = mzDbReader.getMs2SpectrumHeaders()[0];
      DataMode mode = mzDbReader.getSpectrumDataEncoding(firstMS2Spectrum.getSpectrumId()).getMode();
      return (mode == DataMode.FITTED) || (mode == DataMode.CENTROID);
    } catch (SQLiteException e) {
      logger.error("Unable to read first MS1 spectrum encoding", e);
    }
    return false;
  }

  @Override
  public SpectrumData processSpectrum(MgfPrecursor mgfPrecursor, SpectrumData data) {
    return data;
  }

  @Override
  public String getMethodName() {
    return "None";
  }

  @Override
  public String getMethodVersion() {
    return "1.0";
  }
}
