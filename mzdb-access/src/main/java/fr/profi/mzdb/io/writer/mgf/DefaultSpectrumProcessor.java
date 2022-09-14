package fr.profi.mzdb.io.writer.mgf;

import fr.profi.mzdb.model.SpectrumData;

public class DefaultSpectrumProcessor implements ISpectrumProcessor {

  @Override
  public SpectrumData processSpectrum(MgfPrecursor mgfPrecursor, SpectrumData data) {
    return data;
  }
}
