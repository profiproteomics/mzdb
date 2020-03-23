package fr.profi.brucker.timstof.model;

public class Precursor {

    private Integer m_id;
    /*
     *  m/z of the largest (most intensive) peak in this precursor's isotope pattern.
     */
    private Double m_largestPeakMz;

    private Double m_averageMz;
    private Double m_monoIsotopicMz;
    private Integer m_charge;
    /*
    * Mobility (in scan-number units) of this precursor in the corresponding MS^1 frame.
     */
    private Double m_scanNumber;

    private Double m_intensity;

    private Integer m_parentMsFrame;

    public Precursor(Integer id, Double largestPeakMz, Double averageMz, Double monoIsotopicMz, Integer charge, Double scanNumber, Double intensity, Integer parentMsFrame) {
        this.m_id = id;
        this.m_largestPeakMz = largestPeakMz;
        this.m_averageMz =  averageMz;
        this.m_monoIsotopicMz = monoIsotopicMz;
        this.m_charge = charge;
        this.m_scanNumber = scanNumber;
        this.m_intensity = intensity;
        this.m_parentMsFrame = parentMsFrame;
    }

    public Integer getId() {
        return m_id;
    }

    public Double getLargestPeakMz() {
        return m_largestPeakMz;
    }

    public Double getAverageMz() {
        return m_averageMz;
    }

    public Double getMonoIsotopicMz() {
        return m_monoIsotopicMz;
    }

    public Integer getCharge() {
        return m_charge;
    }

    public Double getScanNumber() {
        return m_scanNumber;
    }

    public Double getIntensity() {
        return m_intensity;
    }

    public Integer getParentMsFrame() {
        return m_parentMsFrame;
    }
}
