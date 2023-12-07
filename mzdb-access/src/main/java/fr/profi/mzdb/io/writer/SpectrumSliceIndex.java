package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.model.SpectrumData;

public class SpectrumSliceIndex {

  private SpectrumData spectrumData;
  private float firstPeakIdx;
  private float lastPeakIdx;

  public SpectrumSliceIndex(SpectrumData spectrumData, float firstPeakIdx, float lastPeakIdx) {
    this.spectrumData = spectrumData;
    this.firstPeakIdx = firstPeakIdx;
    this.lastPeakIdx = lastPeakIdx;
  }

  public SpectrumData getSpectrumData() {
    return spectrumData;
  }

  public float getFirstPeakIdx() {
    return firstPeakIdx;
  }

  public float getLastPeakIdx() {
    return lastPeakIdx;
  }

  protected  int peaksCount() {

    if(! (lastPeakIdx >=  firstPeakIdx) ) {
      throw new IllegalArgumentException("invalid pair of firstPeakIdx/lastPeakIdx ("+firstPeakIdx+","+lastPeakIdx+")");
    }

    return (int) ( 1 + lastPeakIdx - firstPeakIdx);
  }

  public void setLastPeakIdx(int i) {
    lastPeakIdx = i;
  }
}
