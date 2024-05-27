package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.BBSizes;
import fr.profi.mzdb.model.RunSliceHeader;

import java.util.*;

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

    //System.out.println("addRunSlice nbSlices="+runSliceById.size());

    int offset = (msLevel == 1) ? 1 : maxMS1SlicesHint * (msLevel-1) +1;

    int runSliceId = (int) (Math.floor(beginMz / bbSizes.BB_MZ_HEIGHT_MS1)) + offset;
    RunSliceHeader runSlice = new RunSliceHeader(runSliceId, msLevel, -1, beginMz, endMz, runId);

    RunSliceKey key = new RunSliceKey(msLevel, beginMz,endMz);
    runSlicesStructure.put(key, runSlice);
    runSliceById.put(runSlice.getId(),runSlice);

    return runSlice;
  }

  public boolean hasRunSlice(Integer msLevel, Float beginMz, Float endMz) {

    _key.msLevel = msLevel;
    _key.beginMz = beginMz;
    _key.endMz = endMz;
    return hasRunSlice(_key);
  }
  private RunSliceKey _key = new RunSliceKey();

  public boolean hasRunSlice(RunSliceKey key) {
    return runSlicesStructure.containsKey( key);
  }

  public Integer getRunSliceId(Integer msLevel, Float beginMz, Float endMz) {

    _key.msLevel = msLevel;
    _key.beginMz = beginMz;
    _key.endMz = endMz;
    if (hasRunSlice(_key)) {
      return runSlicesStructure.get(_key).getId();
    }

    return null;
  }

  public RunSliceHeader getRunSlice(Integer id){
    return runSliceById.get(id);
  }

  public List<RunSliceHeader> getAllRunSlices() {

    // Comparator used to sort according to MsLevel and id.
    Comparator<RunSliceHeader> comparator = new Comparator<>() {
      public int compare(RunSliceHeader rsh1, RunSliceHeader rsh2) {
        if (rsh1.getMsLevel() < rsh2.getMsLevel()) {
          return -1;
        }
        if (rsh1.getMsLevel() > rsh2.getMsLevel()) {
          return 1;
        }

        return rsh1.getId()-rsh2.getId();
      }
    };

    // Set number to all headers in ascending msLevel/id
    List<RunSliceHeader> headers = new ArrayList<RunSliceHeader>(runSlicesStructure.values());
    headers.sort(comparator);
    for (int i=0; i<headers.size(); i++) {
      headers.get(i).setNumber(i+1);
    }

    return headers;
  }

  public Integer getRunSlicesCount() {
    return runSliceById.size();
  }


  private class RunSliceKey {
    Integer msLevel;
    Float beginMz;
    Float endMz;

    public RunSliceKey() {

    }

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
