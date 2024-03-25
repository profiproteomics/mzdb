package fr.profi.bruker.timstof.model;

import it.unimi.dsi.fastutil.doubles.Double2FloatMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntIterator;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import it.unimi.dsi.fastutil.objects.ObjectIterator;
import it.unimi.dsi.fastutil.objects.ObjectList;

import java.util.List;

public class TimsPASEFFrame extends AbstractTimsFrame {

  private Int2ObjectMap<PasefMsMsData> m_pasefMsMsInfoByPrecursor = new Int2ObjectOpenHashMap<>();

  public TimsPASEFFrame(int id, int nbrScans, int nbrPeaks, int scanMode, int maxIntensity, int summedIntensity, double time) {
    super(id, nbrScans, nbrPeaks, scanMode, MsMsType.PASEF, maxIntensity, summedIntensity, time);
  }

  @Override
  public void clearSpectraData() {
    super.clearSpectraData();
    for (PasefMsMsData msmsData : m_pasefMsMsInfoByPrecursor.values()) {
      msmsData.resetSpectrumData();
    }
  }

  public void setMassIntensityByScan(Int2ObjectMap<Double2FloatMap> massIntByScan) {
    IntIterator scansIdIt = massIntByScan.keySet().iterator();
    while (scansIdIt.hasNext()) {
      int scId = scansIdIt.nextInt();
      Double2FloatMap massInt = massIntByScan.get(scId);

      for (PasefMsMsData msmsData : m_pasefMsMsInfoByPrecursor.values()) {
        if (msmsData.containsScan(scId)) {
          //Found PasefMsMs for current scan
          msmsData.addSpectrumData(massInt);
        }
      }
    }
    m_spectrumDataSet = true;
  }

  public IntArrayList getPrecursorIds() {
    if (m_pasefMsMsInfoByPrecursor != null)
      return new IntArrayList(m_pasefMsMsInfoByPrecursor.keySet());
    return null;
  }

  public PasefMsMsData getPrecursorPasefMsMsData(int precursorIndex) {
    if (m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
      return m_pasefMsMsInfoByPrecursor.get(precursorIndex);
    else
      return null;
  }

  public Spectrum getPrecursorSpectrum(int precursorIndex) {
    if (m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
      return m_pasefMsMsInfoByPrecursor.get(precursorIndex).getPasefSpectrum();
    else
      return null;
  }

  public double getPrecursorCollisionEnergy(int precursorIndex) {
    if (m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
      return m_pasefMsMsInfoByPrecursor.get(precursorIndex).getCollisionEnergy();
    else
      return 0.0;
  }

  public ObjectList<Spectrum> getAllSpectra() {
    if (!m_spectrumDataSet)
      return null;

    ObjectList<Spectrum> allSp = new ObjectArrayList<>();
    ObjectIterator<PasefMsMsData> pasefMsMsDataIt = m_pasefMsMsInfoByPrecursor.values().iterator();
    while (pasefMsMsDataIt.hasNext()) {
      allSp.add(pasefMsMsDataIt.next().getPasefSpectrum());
    }
    return allSp;
  }

  @Override
  public int getSpectrumCount() {
    if (m_pasefMsMsInfoByPrecursor != null)
      return m_pasefMsMsInfoByPrecursor.size();
    else
      return 0;
  }


  public void createSingleSpectrum(SpectrumGeneratingMethod msCreateMethod) {
    switch (msCreateMethod){
      case FULL:
        createSingleSpFromAllSpectra();
        break;
      case SMOOTH:
        throw new IllegalArgumentException("Smooth method not supported for PASEF Spectra Frames");
      case MERGED:
        break;
    }

  }


  public List<PasefMsMsData> getPasefMsMSData() {
    return new ObjectArrayList<>(m_pasefMsMsInfoByPrecursor.values());
  }

  public void setPasefMsMsData(List<PasefMsMsData> pasefMsMsInfo) {
    pasefMsMsInfo.forEach(pMsms -> m_pasefMsMsInfoByPrecursor.put(pMsms.getPrecursorId(), pMsms));
  }
}
