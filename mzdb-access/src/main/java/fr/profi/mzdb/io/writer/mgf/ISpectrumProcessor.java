package fr.profi.mzdb.io.writer.mgf;

import fr.profi.mzdb.model.SpectrumData;

public interface ISpectrumProcessor {

  public SpectrumData processSpectrum(MgfPrecursor mgfPrecursor, SpectrumData data);

  public String getMethodName();

  public String getMethodVersion();

}
