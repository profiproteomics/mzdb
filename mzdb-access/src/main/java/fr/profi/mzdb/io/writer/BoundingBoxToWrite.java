package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.IsolationWindow;

import java.util.List;

public class BoundingBoxToWrite {

    Integer id = 0;
    Float firstTime = 0f;
    Float lastTime = 0f;
    Integer runSliceId = 0;
    Integer msLevel = 0;
    DataEncoding dataEncoding  = null;
    IsolationWindow isolationWindow= null;
    List<Long> spectrumIds= null;
    List<SpectrumSliceIndex> spectrumSlices = null; //var spectrumSlices: ArrayBuffer[Option[SpectrumSliceIndex]] = null

    public Integer getId() {
      return id;
    }

    public void setId(Integer id) {
      this.id = id;
    }

    public Float getFirstTime() {
      return firstTime;
    }

    public void setFirstTime(Float firstTime) {
      this.firstTime = firstTime;
    }

    public Float getLastTime() {
      return lastTime;
    }

    public IsolationWindow getIsolationWindow() {
      return isolationWindow;
    }

    public void setIsolationWindow(IsolationWindow isolationWindow) {
      this.isolationWindow = isolationWindow;
    }

    public void setLastTime(Float lastTime) {
      this.lastTime = lastTime;
    }

    public Integer getRunSliceId() {
      return runSliceId;
    }

    public void setRunSliceId(Integer runSliceId) {
      this.runSliceId = runSliceId;
    }

    public Integer getMsLevel() {
      return msLevel;
    }

    public void setMsLevel(Integer msLevel) {
      this.msLevel = msLevel;
    }

    public DataEncoding getDataEncoding() {
      return dataEncoding;
    }

    public void setDataEncoding(DataEncoding dataEncoding) {
      this.dataEncoding = dataEncoding;
    }

    public List<Long> getSpectrumIds() {
      return spectrumIds;
    }

    public void setSpectrumIds(List<Long> spectrumIds) {
      this.spectrumIds = spectrumIds;
    }

    public List<SpectrumSliceIndex> getSpectrumSlices() {
      return spectrumSlices;
    }

    public void setSpectrumSlices(List<SpectrumSliceIndex> spectrumSlices) {
      this.spectrumSlices = spectrumSlices;
    }

}
