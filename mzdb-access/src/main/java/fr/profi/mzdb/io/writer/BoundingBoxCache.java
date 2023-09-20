package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.BBSizes;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.IsolationWindow;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;
import java.util.function.Function;

public class BoundingBoxCache {


  private BBSizes bbSizes;
  private int bbId = 0;
  private HashMap<BoundingBoxMapKey, BoundingBoxToWrite> boundingBoxMap = new HashMap<>();// [Int,Option[IsolationWindow]), BoundingBox]

  public BoundingBoxCache(BBSizes bbs){
    this.bbSizes= bbs;
  }

  public Boolean isTimeForNewBBRow(Integer msLevel, IsolationWindow isolationWindow, Float curSpecTime) { //VDS Removed isolationWindow TODO VERIFY
    Float bbRowFirstSpecTimeOpt = _findBBFirstTime(msLevel, isolationWindow);
    if (bbRowFirstSpecTimeOpt == null)
      return true;

    float maxRtWidth = (msLevel == 1) ?  bbSizes.BB_RT_WIDTH_MS1 :  bbSizes.BB_RT_WIDTH_MSn;
    return (curSpecTime - bbRowFirstSpecTimeOpt) > maxRtWidth;

  }

  private Float _findBBFirstTime(Integer msLevel, IsolationWindow isolationWindow){

    List<Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite>> sortedEntries = new ArrayList<>(boundingBoxMap.entrySet());
    sortedEntries.sort(Map.Entry.comparingByValue(Comparator.comparingInt(BoundingBoxToWrite::getRunSliceId)));
    for(Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite> e : sortedEntries){
      if(Objects.equals(e.getValue().getMsLevel(), msLevel) && Objects.equals(e.getKey().isolationWindow,isolationWindow))
        return e.getValue().getFirstTime();
    }
    return null;
  }

  protected void forEachCachedBB(Integer msLevel, IsolationWindow isolationWindow, Function<BoundingBoxToWrite, Void> fn){ //TODO
    List<Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite>> sortedEntries = new ArrayList<>(boundingBoxMap.entrySet());
    sortedEntries.sort(Map.Entry.comparingByValue(Comparator.comparingInt(BoundingBoxToWrite::getRunSliceId)));
    for(Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite> e : sortedEntries) {

      if(Objects.equals(e.getValue().getMsLevel(), msLevel) && Objects.equals(e.getKey().isolationWindow,isolationWindow)){
        fn.apply(e.getValue());
      }
    }
  }

  public void removeBBRow(Integer msLevel , IsolationWindow isolationWindow) {

    List<Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite>> sortedEntries = new ArrayList<>(boundingBoxMap.entrySet());
    sortedEntries.sort(Map.Entry.comparingByValue(Comparator.comparingInt(BoundingBoxToWrite::getRunSliceId)));
    for(Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite> e : sortedEntries) {
      if(Objects.equals(e.getValue().getMsLevel(), msLevel) && Objects.equals(e.getKey().isolationWindow,isolationWindow)){
        boundingBoxMap.remove(e.getKey());
      }
    }
  }

  public List<Pair<Integer, IsolationWindow>> getBBRowsKeys() {
    List<Pair<Integer, IsolationWindow>> returnedBBRowsKeys = new ArrayList<>();
    List<Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite>> sortedEntries = new ArrayList<>(boundingBoxMap.entrySet());
    sortedEntries.sort(Map.Entry.comparingByValue(Comparator.comparingInt(BoundingBoxToWrite::getRunSliceId)));
    for(Map.Entry<BoundingBoxMapKey, BoundingBoxToWrite> e : sortedEntries) {
      returnedBBRowsKeys.add(new ImmutablePair<>(e.getValue().msLevel, e.getKey().isolationWindow));
    }
    return returnedBBRowsKeys;
  }

  public BoundingBoxToWrite getCachedBoundingBox(Integer runSliceId, IsolationWindow isolationWindow){
    BoundingBoxMapKey k = new BoundingBoxMapKey(runSliceId, isolationWindow);
    return boundingBoxMap.get(k);
  }

  public BoundingBoxToWrite createBoundingBox(Float spectrumTime, Integer runSliceId, Integer msLevel, DataEncoding de, IsolationWindow isolationWindow, Integer slicesCountHint){
    BoundingBoxMapKey k = new BoundingBoxMapKey(runSliceId, isolationWindow);
    assert(! boundingBoxMap.containsKey(k));
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

    boundingBoxMap.put(k,newOrCachedBB);

    return newOrCachedBB;
  }


  private static class BoundingBoxMapKey{
    Integer runSliceId;
    IsolationWindow isolationWindow;

    public BoundingBoxMapKey(Integer runSliceId, IsolationWindow isolationWindow) {
      this.runSliceId = runSliceId;
      this.isolationWindow = isolationWindow;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      BoundingBoxMapKey that = (BoundingBoxMapKey) o;
      return runSliceId.equals(that.runSliceId) &&
              Objects.equals(isolationWindow, that.isolationWindow);
    }

    @Override
    public int hashCode() {
      return Objects.hash(runSliceId, isolationWindow);
    }
  }

}
