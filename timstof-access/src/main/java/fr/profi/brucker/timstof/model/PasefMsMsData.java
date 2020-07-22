package fr.profi.brucker.timstof.model;

import it.unimi.dsi.fastutil.doubles.Double2FloatMap;
import scala.Predef;

import java.text.DecimalFormat;

public class PasefMsMsData {

    private int m_frameId;
    private float m_frameRT;
    private int m_startScan;
    private int m_endScan;
    private int m_precursorId;
    private Spectrum m_pasefMsMs;
    private double m_collisionEnergy;
    private Precursor m_precursor;

   private static DecimalFormat df = new DecimalFormat("#.##");

    /*
    * The isolation m/z
    */
    private double m_isolationMz;

    /*
     * Specifies the total 3-dB width of the isolation window (in m/z units)
     * The center of which is given by 'IsolationMz'.
     */
    private double m_isolationWidth;

    public PasefMsMsData(int frId, int startScan, int endScan, double isolationMz, double isolationWidth, double collisionEnergy, int precursorId, float frameRT){
        this.m_frameId = frId;
        this.m_startScan = startScan;
        this.m_endScan = endScan;
        this.m_isolationMz=isolationMz;
        this.m_isolationWidth=isolationWidth;
        this.m_precursorId = precursorId;
        this.m_collisionEnergy = collisionEnergy;
        this.m_frameRT = frameRT;
    }

    public double getCollisionEnergy() {
        return m_collisionEnergy;
    }

    public int getFrameId() {
        return m_frameId;
    }

    public int getStartScan() {
        return m_startScan;
    }

    public int getEndScan() {
        return m_endScan;
    }

    public int getPrecursorId() {
        return m_precursorId;
    }

    public double getIsolationMz() {
        return m_isolationMz;
    }

    public double getIsolationWidth() {
        return m_isolationWidth;
    }

    protected void setSpectrumData(double[] masses, float[] intensities){
      String avMzStr = PasefMsMsData.df.format(m_precursor.getAverageMz());
      m_pasefMsMs = new Spectrum("Frame_"+m_frameId+"-Precursor_"+avMzStr, 2, m_frameRT, masses, intensities);
    }

    protected void addSpectrumData(Double2FloatMap massInt){
        if(m_pasefMsMs == null) {
          String avMzStr = PasefMsMsData.df.format(m_precursor.getAverageMz());
          m_pasefMsMs = new Spectrum("Frame_" + m_frameId + "-Precursor_" + avMzStr, 2, m_frameRT, massInt);
        } else {
            m_pasefMsMs.addPeaks(massInt);
        }
    }

    protected void addSpectrumData(double[] masses, float[] intensities){
        if(m_pasefMsMs == null) {
          String avMzStr = PasefMsMsData.df.format(m_precursor.getAverageMz());
          m_pasefMsMs = new Spectrum("Frame_" + m_frameId + "-Precursor_" + avMzStr, 2, m_frameRT, masses, intensities);
        } else {
            m_pasefMsMs.addPeaks(masses, intensities);
        }
    }

    public void setPrecursor( Precursor p){
        this.m_precursor = p;
    }

    public Precursor getPrecursor(){
        return m_precursor;
    }

    public Spectrum getPasefSpectrum(){
        return m_pasefMsMs;
    }

    public void resetSpectrumData(){
        m_pasefMsMs = null;
    }

    public boolean containsScan(Integer scanIndex){
        return (scanIndex >= getStartScan() && scanIndex<=getEndScan());
    }
}
