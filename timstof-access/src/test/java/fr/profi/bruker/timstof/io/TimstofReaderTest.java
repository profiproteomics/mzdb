package fr.profi.bruker.timstof.io;

import fr.profi.bruker.timstof.TDFLibrary;
import fr.profi.bruker.timstof.model.*;
import fr.profi.mzdb.model.Peak;
import it.unimi.dsi.fastutil.ints.Int2DoubleMap;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

@Ignore
public class TimstofReaderTest {


  @Test
  public void extractMz() {

    TimstofReader reader = TimstofReader.getTimstofReader();
    Long fileHandler = reader.openTimstofFile(new File("C:\\Local\\bruley\\Data\\TimsTOF\\TP7718MS_Slot1-58_1_7773.d"));


    final List<AbstractTimsFrame> fullTimsFrames = reader.getFullTimsFrames(fileHandler);
    final Int2ObjectMap<Precursor> precursorInfoById = reader.getPrecursorInfoById(fileHandler);

    double precursorMz = 412.7535748; //412.75358;
    final float startRt = 40.1f*60;
    final float endRt = 41.0f*60;

    final List<AbstractTimsFrame> frames = fullTimsFrames.stream().filter(f -> (f.getMsmsType() == AbstractTimsFrame.MsMsType.MS) && (f.getTime() > startRt) && (f.getTime() < endRt)).collect(Collectors.toList());

    final Int2DoubleMap ionMobilities = ionMobilities(reader, fileHandler, 1);
    reader.fillFramesWithSpectrumData(fileHandler, frames);

    System.out.println("frameId; rt; mz; intens; scan; im");
    for(AbstractTimsFrame f : frames) {
      TimsMSFrame frame = (TimsMSFrame) f;

      for (int i = frame.getScanStart(); i <= frame.getScanEnd(); i++) {
        Spectrum s = frame.getScanSpectrum(i);
        if (s != null) {
          final double[] masses = s.getMasses();
          final float[] intensities = s.getIntensities();
          for (int k = 0; k < masses.length; k++) {
            if (1e6 * Math.abs(masses[k] - precursorMz) < 1.0) {
              System.out.println(f.getId() + ";" + f.getTime() + ";" + masses[k] + ";" + intensities[k] + ";" +i+";"+ionMobilities.get(i));
            }
          }
        }
      }
    }

  }

  @Test
  public void rtStats() {

    TimstofReader reader = TimstofReader.getTimstofReader();
    Long fileHandler = reader.openTimstofFile(new File("C:\\Local\\bruley\\Data\\TimsTOF\\TP7718MS_Slot1-58_1_7773.d"));


    final List<AbstractTimsFrame> fullTimsFrames = reader.getFullTimsFrames(fileHandler);

    System.out.println("frameId; rt; type; scan_count; total_scan_count");

    for(AbstractTimsFrame f : fullTimsFrames) {
      List<AbstractTimsFrame> tfs = Collections.singletonList(f);
      reader.fillFramesWithSpectrumData(fileHandler, tfs);
      if (f.getMsmsType() == AbstractTimsFrame.MsMsType.MS) {
        TimsMSFrame msFrame = (TimsMSFrame)f;
        System.out.println(f.getId() + ";" + f.getTime() + ";" + f.getMsmsType() + ";" + msFrame.getScanCount()+";");
      } else {
        TimsPASEFFrame pasefFrame = (TimsPASEFFrame) f;
        int scanCount = pasefFrame.getPasefMsMSData().stream().map(d -> (d.getEndScan() - d.getStartScan() + 1)).collect(Collectors.summingInt(Integer::intValue));
        System.out.println(f.getId() + ";" + f.getTime() + ";" + f.getMsmsType() + ";" + pasefFrame.getSpectrumCount()+";"+scanCount);
      }
    }

  }


  @Test
  public void readMSFrameContent() {

    TimstofReader reader = TimstofReader.getTimstofReader();
//    Long fileHandler = reader.openTimstofFile(new File("C:/Local/bruley/Data/TimsTOF/2022002-1_eColi-10ng-30min_Slot1-10_1_313.d"));
    Long fileHandler = reader.openTimstofFile(new File("C:/Local/bruley/Data/TimsTOF/TP12337RJB_Slot2-45_1_12717.d"));


    final List<AbstractTimsFrame> fullTimsFrames = reader.getFullTimsFrames(fileHandler);
    final Int2ObjectMap<Precursor> precursorInfoById = reader.getPrecursorInfoById(fileHandler);

//    final TimsMSFrame timsFrame = (TimsMSFrame)fullTimsFrames.get(4876-1);
    final TimsMSFrame timsFrame = (TimsMSFrame)fullTimsFrames.get(40378-1);
    reader.fillFramesWithSpectrumData(fileHandler, Collections.singletonList((AbstractTimsFrame) timsFrame));
    System.out.println();

    for (int k = timsFrame.getScanStart(); k <= timsFrame.getScanEnd(); k++) {
      Spectrum msSpectrum = timsFrame.getScanSpectrum(k);
      if(msSpectrum != null) {
        for (int idx = 0; idx < msSpectrum.getMasses().length; idx++) {
          System.out.println(k + ";" + msSpectrum.getMasses()[idx] + ";" + msSpectrum.getIntensities()[idx]);
        }
      } else {
        // System.out.println(k + "; empty scan");
      }

    }


    System.out.println();


    reader.closeTimstofFile(fileHandler);

  }

    @Test
  public void readFrames() {

    TimstofReader reader = TimstofReader.getTimstofReader();
    Long fileHandler = reader.openTimstofFile(new File("C:/Local/bruley/Data/TimsTOF/2022002-1_eColi-10ng-30min_Slot1-10_1_313.d"));


    reader.getIonMobilityIndexes(fileHandler);

    final List<AbstractTimsFrame> fullTimsFrames = reader.getFullTimsFrames(fileHandler);
    final Int2ObjectMap<Precursor> precursorInfoById = reader.getPrecursorInfoById(fileHandler);

    final int precursorId = 22;
    double precursorMz = 457.84750;
    final int precursorFirstFrameId = 16;


    Precursor precursor = precursorInfoById.get(precursorId);

    List<AbstractTimsFrame> tfs = fullTimsFrames.subList(0,40);
    reader.fillFramesWithSpectrumData(fileHandler, tfs);


    // test IM retrieval


    System.out.println();
    System.out.println("Scan to IM calibration");

//    precursorFirstFrameId = 11; //fullTimsFrames.get(fullTimsFrames.size()-1).getId();
//    ionMobilities(reader, fileHandler, precursorFirstFrameId);

    // test MSMSPasef scan for precursor #22

    List<Integer> msmsFrameIds = Arrays.asList(16,17,19,20,21,22);
    List<AbstractTimsFrame> msmsFrames = tfs.stream().filter( f -> msmsFrameIds.contains(f.getId())).collect(Collectors.toList());

    final List<PasefMsMsData> pasefMsMsData = msmsFrames.stream().map(f -> ((TimsPASEFFrame) f).getPrecursorPasefMsMsData(precursorId)).collect(Collectors.toList());

    final List<Spectrum> spectrumList = msmsFrames.stream().map(f -> ((TimsPASEFFrame) f).getPrecursorSpectrum(precursorId)).collect(Collectors.toList());

    Spectrum summedSpectrum = new Spectrum("summed spectrum", 2, (float)msmsFrames.get(0).getTime(), new double[0], new float[0]);

    for(Spectrum s : spectrumList) {
      summedSpectrum.addPeaks(s.getMasses(), s.getIntensities());
    }

    List<Peak> peaks = new ArrayList<>();
    for (int k = 0; k < summedSpectrum.getMasses().length; k++) {
      peaks.add(new Peak(summedSpectrum.getMasses()[k], summedSpectrum.getIntensities()[k]));
    }

    peaks.sort(Comparator.comparingDouble(Peak::getMz));

//    String str = Arrays.stream(summedSpectrum.getMasses()).mapToObj(d -> Double.toString(d)).collect(Collectors.joining("\n", "{", "}"));
//    System.out.println(str);
//    System.out.println();

    // todo : moyenne mz des peaks proches et somme de leur intensites

    for( Peak peak : peaks) {
      System.out.println(peak.getMz()+";"+peak.getIntensity());
    }

//    final TimsMSFrame timsFrame = (TimsMSFrame)fullTimsFrames.get(precursor.getParentMsFrame()-1);
    final TimsMSFrame timsFrame = (TimsMSFrame)fullTimsFrames.get(18-1);

    System.out.println();
    System.out.println("Search precursor mass in MS");


    for (int k = timsFrame.getScanStart(); k <= timsFrame.getScanEnd(); k++) {
      Spectrum msSpectrum = timsFrame.getScanSpectrum(k);
      if(msSpectrum != null) {
        int idx = ~Arrays.binarySearch(msSpectrum.getMasses(), precursorMz);
        if ((idx > 0) && (idx < msSpectrum.getMasses().length)) {
          double dm = Math.abs(msSpectrum.getMasses()[idx] - precursorMz);

          if (idx > 0 && (Math.abs(msSpectrum.getMasses()[idx-1] - precursorMz) < dm)) idx--;

          System.out.println(k + ";" + idx + ";" + msSpectrum.getMasses()[idx] + ";" + msSpectrum.getIntensities()[idx]);
        }
      } else {
        // System.out.println(k + "; empty scan");
      }

    }


    System.out.println();
    System.out.println("convert precursor scan number to IM value");

    ionMobility(reader, fileHandler, precursor.getParentMsFrame(), precursor.getScanNumber());

    reader.closeTimstofFile(fileHandler);

  }

  private Int2DoubleMap ionMobilities(TimstofReader reader, Long fileHandler, int frameId) {
    int n = 1496;
    TDFLibrary m_tdfLib = reader.getTDFLib();
    double[] scanAsDbl = new double [n];
    double[] ionMobilities = new double [n];
    for(int i = 0; i < n; i++){
      scanAsDbl[i] = i;
    }

    long error_stat = m_tdfLib.tims_scannum_to_oneoverk0(fileHandler, frameId, scanAsDbl , ionMobilities, ionMobilities.length);
    if (0 == error_stat) {
      System.out.println(" !!! could not convert scans to ion mobility for frame " + frameId);
    } else{
      Int2DoubleMap map = new Int2DoubleOpenHashMap();

      for(int i =0; i < n; i++){
        map.put((int)scanAsDbl[i], ionMobilities[i]);
        System.out.println(" Scans "+scanAsDbl[i]+" => Ion Mobility "+ionMobilities[i]);
      }
      return map;
    }
    return null;
  }

  private void ionMobility(TimstofReader reader, Long fileHandler, int frameId, double scan) {
    int n = 1496;
    TDFLibrary m_tdfLib = reader.getTDFLib();
    double[] scanAsDbl = {scan};
    double[] ionMobilities = new double [1];

    long error_stat = m_tdfLib.tims_scannum_to_oneoverk0(fileHandler, frameId, scanAsDbl , ionMobilities, ionMobilities.length);
    if (0 == error_stat) {
      System.out.println(" !!! could not convert scans to ion mobility for frame " + frameId);
    } else{
        System.out.println(" Scans "+scan+" => Ion Mobility "+ionMobilities[0]);
    }
  }

}
