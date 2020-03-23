package fr.profi.brucker.timstof.model;

public class PasefMsMsData {

    private Integer m_frameId;
    private float m_frameRT;
    private Integer m_startScan;
    private Integer m_endScan;
    private Integer m_precursorId;
    private Spectrum m_pasefMsMs;
    private double m_collisionEnergy;
    private Precursor m_precursor;


    /*
    * The isolation m/z
    */
    private Double m_isolationMz;

    /*
     * Specifies the total 3-dB width of the isolation window (in m/z units)
     * The center of which is given by 'IsolationMz'.
     */
    private Double m_isolationWidth;

    public PasefMsMsData(Integer frId, Integer startScan, Integer endScan, Double isolationMz, Double isolationWidth, double collisionEnergy, Integer precursorId, float frameRT){
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

    public Integer getFrameId() {
        return m_frameId;
    }

    public Integer getStartScan() {
        return m_startScan;
    }

    public Integer getEndScan() {
        return m_endScan;
    }

    public Integer getPrecursorId() {
        return m_precursorId;
    }

    public Double getIsolationMz() {
        return m_isolationMz;
    }

    public Double getIsolationWidth() {
        return m_isolationWidth;
    }

    protected void setSpectrumData(double[] masses, float[] intensities){
        m_pasefMsMs = new Spectrum("Frame_"+m_frameId+"-Precursor_"+m_precursor.getScanNumber(), 2, m_frameRT, masses, intensities);
    }

    protected void addSpectrumData(double[] masses, float[] intensities){
        if(m_pasefMsMs == null)
            m_pasefMsMs = new Spectrum("Frame_"+m_frameId+"-Precursor_"+m_precursor.getScanNumber(), 2, m_frameRT, masses, intensities);
        else {
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
