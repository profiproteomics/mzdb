package fr.profi.brucker.timstof.converter;

import com.beust.jcommander.IStringConverter;
import com.beust.jcommander.Parameter;

public class ConverterArguments {


  public class MS1MethodConverter implements IStringConverter<SpectrumGeneratingMethod> {

    @Override
    public SpectrumGeneratingMethod convert(String s) {
      return SpectrumGeneratingMethod.valueOf(s);
    }
  }

  @Parameter(names = "-ms1", description = "MS1 spectrum generation method. Full: keep all peaks (for same moz, keep most intense); merged: group peaks by moz at 10ppm (keep most intense value); smooth: smooth the spectra, detect max points and keep peaks for these points using original intensity", required = false, converter = MS1MethodConverter.class)
  SpectrumGeneratingMethod ms1;

  @Parameter(names = {"-f","--file"}, description = "timstof file to convert", required = false)
  String filename;

}
