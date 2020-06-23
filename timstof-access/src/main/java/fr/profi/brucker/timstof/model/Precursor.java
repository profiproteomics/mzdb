package fr.profi.brucker.timstof.model;

public class Precursor {

    private int m_id;
    /*
     *  m/z of the largest (most intensive) peak in this precursor's isotope pattern.
     */
    private double m_largestPeakMz;

    private double m_averageMz;
    private double m_monoIsotopicMz;
    private int m_charge;
    /*
    * Mobility (in scan-number units) of this precursor in the corresponding MS^1 frame.
     */
    private double m_scanNumber;

    private double m_intensity;

    private int m_parentMsFrame;

    public Precursor(int id, double largestPeakMz, double averageMz, double monoIsotopicMz, int charge, double scanNumber, double intensity, int parentMsFrame) {
        this.m_id = id;
        this.m_largestPeakMz = largestPeakMz;
        this.m_averageMz =  averageMz;
        this.m_monoIsotopicMz = monoIsotopicMz;
        this.m_charge = charge;
        this.m_scanNumber = scanNumber;
        this.m_intensity = intensity;
        this.m_parentMsFrame = parentMsFrame;
    }

    public int getId() {
        return m_id;
    }

    public double getLargestPeakMz() {
        return m_largestPeakMz;
    }

    public double getAverageMz() {
        return m_averageMz;
    }

    public double getMonoIsotopicMz() {
        return m_monoIsotopicMz;
    }

    public int getCharge() {
        return m_charge;
    }

    public double getScanNumber() {
        return m_scanNumber;
    }

    public double getIntensity() {
        return m_intensity;
    }

    public int getParentMsFrame() {
        return m_parentMsFrame;
    }
}
