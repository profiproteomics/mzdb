package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.BBSizes;
import fr.profi.mzdb.model.RunSliceHeader;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class RunSliceStructureFactory {

  private final BBSizes bbSizes;
  private final int maxMS1SlicesHint;
  private HashMap<RunSliceKey, RunSliceHeader> runSlicesStructure = new HashMap<>();
  private HashMap<Integer, RunSliceHeader> runSliceById = new HashMap<>();
  private final int runId;

  public RunSliceStructureFactory(Integer runId, int maxMS1SlicesHint, BBSizes bbSizes){
    this.runId = runId;
    this.bbSizes = bbSizes;
    this.maxMS1SlicesHint = maxMS1SlicesHint;
  }

  public RunSliceHeader addRunSlice(Integer msLevel, Float beginMz, Float endMz){
    int offset = (msLevel == 1) ? 1 : maxMS1SlicesHint + 1;
    int runSliceId = (int) (Math.floor(beginMz / bbSizes.BB_MZ_HEIGHT_MS1)) + offset;
    RunSliceHeader runSlice = new RunSliceHeader(runSliceId, msLevel, -1, beginMz, endMz, runId);
    fr.profi.mzdb.io.writer.RunSliceStructureFactory.RunSliceKey key = new RunSliceKey(msLevel, beginMz,endMz);
    runSlicesStructure.put(key, runSlice);
    runSliceById.put(runSlice.getId(),runSlice);
    return runSlice;
  }

  public boolean hasRunSlice(Integer msLevel, Float beginMz, Float endMz) {
    return hasRunSlice(new RunSliceKey(msLevel, beginMz, endMz));
  }

  public boolean hasRunSlice(RunSliceKey key) {
    return runSlicesStructure.containsKey( key);
  }

  public Integer getRunSliceId(Integer msLevel, Float beginMz, Float endMz) {
    RunSliceKey k = new RunSliceKey(msLevel, beginMz,endMz);
    if(hasRunSlice(k))
      return runSlicesStructure.get(k).getId();
    else
      return null;
  }

  public RunSliceHeader getRunSlice(Integer id){
    return runSliceById.get(id);
  }

  public List<RunSliceHeader> getAllRunSlices() {
    AtomicInteger finalRunSliceNumber = new AtomicInteger(1);
    List<RunSliceHeader> headers = new ArrayList<>();
    Map<Integer, List<RunSliceHeader>> runSlicesHeaderByMsLevel = runSlicesStructure.values().stream().collect(Collectors.groupingBy(RunSliceHeader::getMsLevel));
    List<Integer> msLevels = runSlicesHeaderByMsLevel.keySet().stream().collect(Collectors.toList());
    Collections.sort(msLevels);
    for(Integer msL : msLevels){
      List<RunSliceHeader> currentHeaders = runSlicesHeaderByMsLevel.get(msL);
      currentHeaders.sort(Comparator.comparing(o -> Integer.valueOf(o.getId())));
      currentHeaders.forEach(rsH ->{
        RunSliceHeader newRsH = new RunSliceHeader(rsH.getId(), rsH.getMsLevel(), finalRunSliceNumber.getAndIncrement(), rsH.getBeginMz(), rsH.getEndMz(),rsH.getRunId());
        headers.add(newRsH);
      });
    }

    return headers;
  }

  public Integer getRunSlicesCount(){
    return runSliceById.size();
  }


  private class RunSliceKey {
    Integer msLevel;
    Float beginMz;
    Float endMz;

    public RunSliceKey(Integer msL, Float beginMz, Float endMz){
      this.msLevel = msL;
      this.beginMz = beginMz;
      this.endMz = endMz;
    }

    public Integer getMsLevel() {
      return msLevel;
    }

    public Float getBeginMz() {
      return beginMz;
    }

    public Float getEndMz() {
      return endMz;
    }

   @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      RunSliceKey that = (RunSliceKey) o;
      return Objects.equals(getMsLevel(), that.getMsLevel()) &&
              Objects.equals(getBeginMz(), that.getBeginMz()) &&
              Objects.equals(getEndMz(), that.getEndMz());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getMsLevel(), getBeginMz(), getEndMz());
    }
  }

}
