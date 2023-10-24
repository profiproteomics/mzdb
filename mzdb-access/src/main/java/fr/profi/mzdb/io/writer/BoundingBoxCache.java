package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.BBSizes;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.IsolationWindow;

import java.util.*;
import java.util.function.Function;

public class BoundingBoxCache {


  private BBSizes bbSizes;
  private int bbId = 0;

  private HashMap<MsLevelAndIsolationWindowKey, TreeMap<Integer, BoundingBoxToWrite>> level2BoundingBoxMap = new HashMap<>();
  private HashMap<MsLevelAndIsolationWindowKey, Float> firstTimeMap = new HashMap<>();

  public BoundingBoxCache(BBSizes bbs){
    this.bbSizes = bbs;
  }

  public Boolean isTimeForNewBBRow(Integer msLevel, IsolationWindow isolationWindow, Float curSpecTime) {
    Float bbRowFirstSpecTime = _findBBFirstTime(msLevel, isolationWindow);
    if (bbRowFirstSpecTime == null) {
      return true;
    }

    float maxRtWidth = (msLevel == 1) ?  bbSizes.BB_RT_WIDTH_MS1 :  bbSizes.BB_RT_WIDTH_MSn;
    return (curSpecTime - bbRowFirstSpecTime) > maxRtWidth;
  }

  /**
   * Find the smallest firstTime of a Bounding Box with the corresponding value of msLevel and isolationWindow (can be null)
   * @param msLevel
   * @param isolationWindow
   * @return
   */
  private Float _findBBFirstTime(Integer msLevel, IsolationWindow isolationWindow) {

    msLevelAndIsolationWindowKey.msLevel = msLevel;
    msLevelAndIsolationWindowKey.isolationWindow = isolationWindow;
    Float firstTime = firstTimeMap.get(msLevelAndIsolationWindowKey);

    return firstTime; // can be null

  }
  private static final MsLevelAndIsolationWindowKey msLevelAndIsolationWindowKey = new MsLevelAndIsolationWindowKey();

  protected void forEachCachedBB(Integer msLevel, IsolationWindow isolationWindow, Function<BoundingBoxToWrite, Void> fn){

    msLevelAndIsolationWindowKey.msLevel = msLevel;
    msLevelAndIsolationWindowKey.isolationWindow = isolationWindow;

    TreeMap<Integer, BoundingBoxToWrite> map = level2BoundingBoxMap.get(msLevelAndIsolationWindowKey);
    if (map != null) {
      for (BoundingBoxToWrite boundingBoxToWrite : map.values()) {
        fn.apply(boundingBoxToWrite);
      }
    }

  }

  protected BoundingBoxToWrite hasOneCachedBBWithOneSpectrum(Integer msLevel, IsolationWindow isolationWindow) {
    msLevelAndIsolationWindowKey.msLevel = msLevel;
    msLevelAndIsolationWindowKey.isolationWindow = isolationWindow;

    TreeMap<Integer, BoundingBoxToWrite> map = level2BoundingBoxMap.get(msLevelAndIsolationWindowKey);
    if (map == null) {
      return null;
    }
    if (map.size() == 1) {
      BoundingBoxToWrite bb =  map.values().iterator().next();
      List<Long> spectrumIds = bb.getSpectrumIds();
      if (spectrumIds.size() == 1) {
        return bb;
      }
    }
    return null;
  }

  public void removeBBRow(Integer msLevel , IsolationWindow isolationWindow) {

    msLevelAndIsolationWindowKey.msLevel = msLevel;
    msLevelAndIsolationWindowKey.isolationWindow = isolationWindow;
    level2BoundingBoxMap.remove(msLevelAndIsolationWindowKey);
    firstTimeMap.remove(msLevelAndIsolationWindowKey);

  }

  public Set<MsLevelAndIsolationWindowKey> getBBRowsKeys() {
    return new HashSet<MsLevelAndIsolationWindowKey>(level2BoundingBoxMap.keySet());
  }

  public BoundingBoxToWrite getCachedBoundingBox(Integer msLevel, Integer runSliceId, IsolationWindow isolationWindow) {
    msLevelAndIsolationWindowKey.msLevel = msLevel;
    msLevelAndIsolationWindowKey.isolationWindow = isolationWindow;
    TreeMap<Integer, BoundingBoxToWrite> map = level2BoundingBoxMap.get(msLevelAndIsolationWindowKey);
    if (map == null) {
      return null;
    }
    BoundingBoxToWrite boundingBoxToWrite = map.get(runSliceId);

    return boundingBoxToWrite;
  }

  public BoundingBoxToWrite createBoundingBox(Float spectrumTime, Integer runSliceId, Integer msLevel, DataEncoding de, IsolationWindow isolationWindow, Integer slicesCountHint){

    msLevelAndIsolationWindowKey.msLevel = msLevel;
    msLevelAndIsolationWindowKey.isolationWindow = isolationWindow;

    TreeMap<Integer, BoundingBoxToWrite> map = level2BoundingBoxMap.get(msLevelAndIsolationWindowKey);


    if (map == null) {
      map = new TreeMap<Integer, BoundingBoxToWrite>();
      MsLevelAndIsolationWindowKey key = new MsLevelAndIsolationWindowKey();
      key.msLevel = msLevel;
      key.isolationWindow = isolationWindow;
      level2BoundingBoxMap.put(key, map);
      firstTimeMap.put(key, spectrumTime);
    } else {
      Float firstTime = firstTimeMap.get(msLevelAndIsolationWindowKey);
      if (firstTime>spectrumTime) {
        MsLevelAndIsolationWindowKey key = new MsLevelAndIsolationWindowKey();
        key.msLevel = msLevel;
        key.isolationWindow = isolationWindow;
        firstTimeMap.put(key, spectrumTime);
      }
    }

    bbId ++;
    BoundingBoxToWrite newOrCachedBB = new BoundingBoxToWrite();
    newOrCachedBB.setSpectrumIds(new ArrayList<>(slicesCountHint));
    newOrCachedBB.setSpectrumSlices(new ArrayList<>(slicesCountHint));
    newOrCachedBB.setId(bbId);
    newOrCachedBB.setFirstTime(spectrumTime);
    newOrCachedBB.setLastTime(spectrumTime);
    newOrCachedBB.setRunSliceId(runSliceId);
    newOrCachedBB.setMsLevel(msLevel);
    newOrCachedBB.setDataEncoding(de);
    newOrCachedBB.setIsolationWindow(isolationWindow);

    map.put(runSliceId,newOrCachedBB);

    return newOrCachedBB;
  }



  public void dump() {
/*
        int nbLevel1 = 0;
        int nbLevel2 = 0;
        int nbLevelSup = 0;
        for (Map.Entry<MsLevelAndIsolationWindowKey, TreeMap<Integer, BoundingBoxToWrite>> entries : level2BoundingBoxMap.entrySet()) {
            MsLevelAndIsolationWindowKey key = entries.getKey();
            TreeMap map = entries.getValue();
            if (key.msLevel == 1) {
                nbLevel1 = map.size();
            } else if  (key.msLevel == 2) {
                nbLevel2 = map.size();
            } else {
                nbLevelSup = map.size();
            }

        }
        int nbBB = nbLevel1+nbLevel2+nbLevelSup;

        System.out.println("BBbox nb="+nbBB+"   level1="+nbLevel1+"   level2="+nbLevel2+"   levelSup="+nbLevelSup);*/
  }


  public static class MsLevelAndIsolationWindowKey {
    public int msLevel;
    public IsolationWindow isolationWindow = null;

    @Override
    public boolean equals(Object obj) {
      if(obj != null && obj instanceof MsLevelAndIsolationWindowKey) {
        MsLevelAndIsolationWindowKey s = (MsLevelAndIsolationWindowKey) obj;

        if (s.msLevel != msLevel) {
          return false;
        }

        return Objects.equals(isolationWindow, s.isolationWindow);
      }
      return false;
    }

    @Override
    public int hashCode() {
      if (isolationWindow == null) {
        return msLevel * 31;
      } else {
        return msLevel * 31 +isolationWindow.hashCode();
      }
    }
  }
}
