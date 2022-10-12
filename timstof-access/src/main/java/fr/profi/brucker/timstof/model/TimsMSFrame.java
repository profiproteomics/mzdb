package fr.profi.brucker.timstof.model;

import fr.profi.mzdb.algo.signal.filtering.SavitzkyGolaySmoother;
import fr.profi.mzdb.algo.signal.filtering.SavitzkyGolaySmoothingConfig;
import fr.profi.mzdb.util.math.DerivativeAnalysis;
import it.unimi.dsi.fastutil.doubles.Double2FloatMap;
import it.unimi.dsi.fastutil.doubles.Double2FloatOpenHashMap;
import it.unimi.dsi.fastutil.doubles.DoubleArrayList;
import it.unimi.dsi.fastutil.doubles.DoubleComparators;
import it.unimi.dsi.fastutil.floats.Float2ObjectMap;
import it.unimi.dsi.fastutil.floats.Float2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.floats.FloatArrayList;
import it.unimi.dsi.fastutil.floats.FloatComparators;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntIterator;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import it.unimi.dsi.fastutil.objects.ObjectIterator;
import it.unimi.dsi.fastutil.objects.ObjectList;
import scala.Tuple2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

public class TimsMSFrame extends AbstractTimsFrame{

  private static Boolean SMOOTH_SUM = Boolean.valueOf(System.getProperty("smooth.sum", "FALSE"));
  private static Boolean SMOOTH_SUM_BOUNDED = Boolean.valueOf(System.getProperty("smooth.sum.bounded", "FALSE"));


  private Int2ObjectMap<Spectrum> m_spectrumByScan;
  private Float2ObjectMap<DoubleArrayList> m_allIntensity2Masses; //Used to test create SingleSpectrum grouping nearest massed and using higher intensity
  private Double2FloatMap m_mass2retainedIntensityMap; // keep all masses and higher intensity. USe Smoother and max point to create SingleSpectrum

  private int scanStart = Integer.MAX_VALUE;

  private int scanEnd = Integer.MIN_VALUE;

  public TimsMSFrame(int id, int nbrScans, int nbrPeaks, int scanMode, int maxIntensity, int summedIntensity, double time) {
    super(id, nbrScans, nbrPeaks, scanMode, MsMsType.MS, maxIntensity, summedIntensity, time);
  }

  @Override
  public void clearSpectraData() {
    super.clearSpectraData();
    if(m_singleSpectrum !=null)
      m_singleSpectrum = null;
    if(m_spectrumByScan != null)
      m_spectrumByScan.clear();

    //For merged method
    if(m_allIntensity2Masses!=null)
      m_allIntensity2Masses.clear();

    //for smooth method
    if(m_mass2retainedIntensityMap != null)
      m_mass2retainedIntensityMap.clear();

  }

  public void setMassIntensityByScan(Int2ObjectMap<Double2FloatMap> massIntByScan){
    IntIterator scansIdIt= massIntByScan.keySet().iterator();
    int totalIndex = 0;
    while (scansIdIt.hasNext()){
      int scId = scansIdIt.nextInt();
      Double2FloatMap massInt = massIntByScan.get(scId);

        if(m_spectrumByScan == null)
          m_spectrumByScan = new Int2ObjectOpenHashMap<>();
        //For merged method
        if(m_allIntensity2Masses == null)
            m_allIntensity2Masses = new Float2ObjectOpenHashMap<>();
        //for smooth method
        if(m_mass2retainedIntensityMap == null)
          m_mass2retainedIntensityMap = new Double2FloatOpenHashMap();

        double[] scanMasses = new double[massInt.size()];
        float[] scanIntensities = new float[massInt.size()];
        ObjectIterator<Double2FloatMap.Entry> entries = massInt.double2FloatEntrySet().iterator();
        Double2FloatMap.Entry dataEntry;
        int index = 0;
        while (entries.hasNext()){
          dataEntry = entries.next();
          double massVal = dataEntry.getDoubleKey();
          float intensityVal = dataEntry.getFloatValue();
          scanMasses[index] = massVal;
          scanIntensities[index++] = intensityVal;

          //For merged method
          DoubleArrayList masses4Intensity = m_allIntensity2Masses.getOrDefault(intensityVal, new DoubleArrayList());
          masses4Intensity.add(massVal);
          m_allIntensity2Masses.put(intensityVal, masses4Intensity);

          //for smooth method
          float retainedIntensity = m_mass2retainedIntensityMap.getOrDefault(massVal, 0);
          if (!SMOOTH_SUM) {
            if (retainedIntensity < intensityVal) {
              m_mass2retainedIntensityMap.put(massVal, intensityVal);
            }
          } else {
            m_mass2retainedIntensityMap.put(massVal, retainedIntensity + intensityVal);
          }
        }
        totalIndex += index;
        scanStart = Math.min(scanStart, scId);
        scanEnd = Math.max(scanEnd, scId);

        //Keep m_spectrumByScan for MS ?
        m_spectrumByScan.put(scId, new Spectrum("Frame_"+m_id+"-scan_"+scId,1, (float)getTime(), massInt));
      }

    m_spectrumDataSet = true;
  }

  public Spectrum getScanSpectrum(int scanId){
    if(!m_spectrumDataSet ||!m_spectrumByScan.containsKey(scanId))
      return null;

      return m_spectrumByScan.get(scanId);
    }

  public ObjectList<Spectrum> getAllSpectra(){
    if(!m_spectrumDataSet)
      return null;

      return new ObjectArrayList<>(m_spectrumByScan.values());
  }

  @Override
  protected void createSingleSpectrum(SpectrumGeneratingMethod msCreateMethod) {
    switch (msCreateMethod){
      case FULL:
        createSingleSpFromAllSpectra();
        break;
      case MERGED:
        createMergedSingleSpFromAllIntensitiesMap();
        break;
      case SMOOTH:
        createSingleSpectrumFromMassIntensityMap();
        break;
    }

  }


  public int getScanStart() {
    return scanStart;
  }

  public int getScanEnd() {
    return scanEnd;
  }

  public int getScanCount() {
    return m_spectrumByScan.size();
  }
  @Override
  public int getSpectrumCount(){
    return 1;
  }

  //-- VDS For Duration LOG
  static int nbrSp =0;
  static long time0  =0;
  static long time01 =0;
  static long time02  =0;
  static long time03 =0;
  static long time04 =0;

  //For SMOOTH method
  private void createSingleSpectrumFromMassIntensityMap(){
    long start = System.currentTimeMillis();//-- VDS For Duration LOG
    nbrSp++;
    int nbrPeakMS = 0;
    double[] newSpMasses = new double[0];
    float[] newSpIntensities = new  float[0];
    if(m_mass2retainedIntensityMap != null) {
      DoubleArrayList allMasses = new DoubleArrayList(m_mass2retainedIntensityMap.keySet());
      nbrPeakMS = allMasses.size();
      allMasses.sort(DoubleComparators.NATURAL_COMPARATOR);
      //-- VDS For Duration LOG
      long s1 = System.currentTimeMillis();
      time0 = s1-start;

      Tuple2[] allPeaks = new Tuple2[allMasses.size()];
      for (int index = 0; index < allMasses.size(); index++) {
        double nextmass = allMasses.getDouble(index);
        float nextIntensity = m_mass2retainedIntensityMap.get(nextmass);
        allPeaks[index] = new Tuple2(nextmass, (double) nextIntensity);
      }//end go through masses
      //-- VDS For Duration LOG
      long s2 = System.currentTimeMillis();
      time01 = s2-s1;

      //Smooth Signal : same nbr peaks than original spectrum
      SavitzkyGolaySmoother smoother = new SavitzkyGolaySmoother(new SavitzkyGolaySmoothingConfig(3, 2, 1));
      Tuple2[] smoothedSpectrum = smoother.smoothTimeIntensityPairs(allPeaks);
      //-- VDS For Duration LOG
      long s3 = System.currentTimeMillis();
      time02 = s3-s2;

      int resultLength = smoothedSpectrum.length;
      double[] smoothedMasses = new double[resultLength];
      double[]  smoothedIntensities = new double[resultLength];
      for (int k = 0; k < resultLength; k++) {
        smoothedMasses[k] = (Double)smoothedSpectrum[k]._1;
        smoothedIntensities[k] = (Double)smoothedSpectrum[k]._2;
      }

      //Get Max values on smoothedSignal
      DerivativeAnalysis.ILocalDerivativeChange[] mm = DerivativeAnalysis.findMiniMaxi(smoothedIntensities);
      //-- VDS For Duration LOG
      long s4 = System.currentTimeMillis();
      time03 = s4-s3;

      int realLenght = 0;
      newSpMasses = new double[mm.length];
      newSpIntensities = new float[mm.length];
      for (int k = 0; k < mm.length; k++) {
        if(mm[k].isMaximum()){
          double mass = smoothedMasses[mm[k].index()];
          double intensity =(Double) allPeaks[mm[k].index()]._2;

          if (SMOOTH_SUM) {
            int shift = mm[k].index() - 1;
            int mmPrevIndex = ((k > 0) && SMOOTH_SUM_BOUNDED) ? mm[k - 1].index() : 0;
            while (shift > 0 && shift >= mmPrevIndex && (Math.abs(smoothedMasses[shift] - mass) / mass * 1000000) < 10) {
              intensity += (Double) allPeaks[shift]._2;
              shift--;
            }
            shift = mm[k].index() + 1;
            int mmNextIndex = (k < (mm.length - 1) && SMOOTH_SUM_BOUNDED) ? mm[k + 1].index() : allPeaks.length - 1;
            while (shift < allPeaks.length && shift <= mmNextIndex && (Math.abs(smoothedMasses[shift] - mass) / mass * 1000000) < 10) {
              intensity += (Double) allPeaks[shift]._2;
              shift++;
            }
          }


          newSpMasses[realLenght] = mass;
          newSpIntensities[realLenght] = (float) intensity;
          realLenght++;
        }
      }
      newSpMasses  =  Arrays.copyOfRange(newSpMasses, 0, realLenght);
      newSpIntensities  =  Arrays.copyOfRange(newSpIntensities, 0, realLenght);
      //-- VDS For Duration LOG
      long s5 = System.currentTimeMillis();
      time04 = s5-s4;
    }

    if (newSpMasses.length == 0)
      LOG.debug("SingleSpectrum >>> Frame_" + m_id + " has\t" + nbrPeakMS + "\tpeaks, reduced to\t" + newSpMasses.length);

    m_singleSpectrum = new Spectrum("Frame_" + m_id, 1, (float) m_time, newSpMasses, newSpIntensities);
    //-- VDS For Duration LOG
    if(nbrSp%100 == 0){
      LOG.debug( " SMOOTH SingleSpectrum>> Frame Single READ Duration: {} reduced to {} ",nbrPeakMS,newSpMasses.length);
      LOG.debug( " SingleSpectrum>> Frame  Sort Masses: "+time0);
      LOG.debug( " SingleSpectrum>> Frame  Create Tuple: "+time01);
      LOG.debug( " SingleSpectrum>> Frame  Smooth: "+time02);
      LOG.debug( " SingleSpectrum>> Frame  Create [] and Get MinMax: "+time03);
      LOG.debug( " SingleSpectrum>> Frame  Create final []: "+time04);
      time0=0;
      time01=0;
      time02=0;
      time03=0;
      time04=0;
    }
  }

    //For MERGED method
  private void createMergedSingleSpFromAllIntensitiesMap(){
        long start = System.currentTimeMillis();
        nbrSp++;
        Double2FloatMap retainedMasses2Intensity = new Double2FloatOpenHashMap();
        int nbrPeakMS = 0;
        if(m_allIntensity2Masses != null) {
            FloatArrayList allIntensities = new FloatArrayList(m_allIntensity2Masses.keySet());
            allIntensities.sort(FloatComparators.OPPOSITE_COMPARATOR);
            ArrayList<Double> keyMasses = new ArrayList<>();

            long s1 = System.currentTimeMillis();
            time0 += (s1 - start);

            for (float nextIntensity : allIntensities) {
                DoubleArrayList allMasses = m_allIntensity2Masses.get(nextIntensity);
                for (double nextMass : allMasses) {
                    start = System.currentTimeMillis();
                    nbrPeakMS++;
                    if (keyMasses.isEmpty()) {
                        keyMasses.add(nextMass);
                        retainedMasses2Intensity.put(nextMass, nextIntensity);
                    } else {
                        int index = Collections.binarySearch(keyMasses, nextMass);
                        long s2 = System.currentTimeMillis();
                        time01 += (s2 - start);
                        if (index < 0) {
                            index = ~index;
                            int idxToInsert = index;
                            double min = Double.MAX_VALUE;
                            for (int k = Math.max(0, index - 1); k <= Math.min(keyMasses.size() - 1, index + 1); k++) {
                                if (Math.abs(keyMasses.get(k) - nextMass) < min) {
                                    min = Math.abs(keyMasses.get(k) - nextMass);
                                    index = k;
                                }
                            }
                            long s3 = System.currentTimeMillis();
                            time02 += (s3 - s2);
                            //found nearest mass, test if < than 10 ppm
                            double nearestMass = keyMasses.get(index);
                            double asPpm = Math.abs(nearestMass - nextMass) / nextMass * 1000000;
                            if (asPpm > 10.0) { // new mass. insert it. Otherwise merge mass and previously entered intensity should be the best.
                                keyMasses.add(idxToInsert, nextMass);
                                retainedMasses2Intensity.put(nextMass, nextIntensity);
                                long s4 = System.currentTimeMillis();
                                time03 += (s4 - s3);
                            }
                        }//else don't do anything. mass already in map with the best intensity(intensity are ordered so the best is the first found, used to create the entry)
                    }
                }//end go through masses for intensity
            }//end go through intensities
        }

        m_singleSpectrum = new Spectrum("Frame_" + m_id, 1, (float) m_time, retainedMasses2Intensity);
        if(nbrSp%1000 == 0){
            LOG.debug( " MERGED SingleSpectrum>> Frame Single READ Duration: {} reduced to {} ",nbrPeakMS,retainedMasses2Intensity.size());
            LOG.debug( " SingleSpectrum>> Frame Single READ Duration: ");
            LOG.debug( " SingleSpectrum>> Frame  Sort intensities: "+time0);
            LOG.debug( " SingleSpectrum>> Frame  Binary search: "+time01);
            LOG.debug( " SingleSpectrum>> Frame  Find nearest: "+time02);
            LOG.debug( " SingleSpectrum>> Frame  inset new mass: "+time03);
            time0=0;
            time01=0;
            time02=0;
            time03=0;
        }
    }
}
