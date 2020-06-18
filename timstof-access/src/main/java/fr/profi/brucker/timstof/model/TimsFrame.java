package fr.profi.brucker.timstof.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;


/* TODO Create sub classes for PASEF Frame / MS Frame etc instead of MsmsType test in code ::
*
 */
public class TimsFrame implements Comparable<TimsFrame> {
    private final static Logger LOG = LoggerFactory.getLogger(TimsFrame.class);
    private Integer m_id;
    private Integer m_nbrScans;
    private Integer m_nbrPeaks;

    /**
    --   0 = MS
    --   1 = AutoMSMS
    --   2 = MRM
    --   3 = in-source CID
    --   4 = broadband CID
    --   8 = PASEF
    --   9 = DIA
    */
    private ScanMode m_scanMode;

    /**
    --   0 = MS frame
    --   2 = MS/MS fragment frame
    --   8 = PASEF frame
    --   9 = DIA frame
    */
    private MsMsType m_msmsType;

    private boolean m_isPasef;
    /**
     * Time (in seconds), relative to the start time of the acquisition.
     */
    private Double m_time;

    /**
     * Maximum intensity occurring in all data belonging to this frame (can quickly generate a BPC from this)
     */
    private Integer m_maxIntensity;

    /**
     * Sum of all intensities occurring in the data belonging to this frame (can quickly generate a TIC from this)
     */
    private Integer m_summedIntensity;

   // private Map<Integer, Map<Double, Float>> m_massIntensityByScan;

    //PASEF Frame specific
    private Map<Integer, PasefMsMsData> m_pasefMsMsInfoByPrecursor = new HashMap<>();

    //MS frame specific
    private Map<Integer, Spectrum> m_spectrumByScan;
    private boolean m_spectrumDataSet =false;

    private Spectrum m_singleSpectrum = null;

    /* a negative integer, zero, or a positive integer as this object
     * is less than, equal to, or greater than the specified object.
     */
    @Override
    public int compareTo(TimsFrame o) {
        if(o==null)
            return 1;
        if( this.getTime() < o.getTime())
            return  -1;
        else if( this.getTime() > o.getTime())
            return 1;
        else
            return 0;
    }



    public  enum  MsMsType {
        MS(0),
        MSMS(2),
        PASEF(8),
        DIA(9);

        Integer m_msmsTypeCode;

        MsMsType(Integer msmsTypeCode){
            this.m_msmsTypeCode=msmsTypeCode;
        }

        public Integer getMsMsTypeCode(){
            return this.m_msmsTypeCode;
        }

        public static MsMsType findByCode(Integer code){
            MsMsType[] allValues = MsMsType.values();
            for(MsMsType type : allValues){
                if(type.getMsMsTypeCode().equals(code)){
                    return type;
                }
            }
            return  null;
        }

    }

    public  enum  ScanMode {
        MS(0),
        AutoMSMS(1),
        MRM(2),
        IN_SOURCE_CID(3),
        BROADBAND_CID(4),
        PASEF(8),
        DIA(9);

        Integer m_modeCode;

        ScanMode(Integer modeCode){
            this.m_modeCode=modeCode;
        }

        public Integer getScanModeCode(){
            return this.m_modeCode;
        }

        public static ScanMode findByCode(Integer code){
            ScanMode[] allValues = ScanMode.values();
            for(ScanMode sm : allValues){
                if(sm.getScanModeCode().equals(code)){
                    return sm;
                }
            }
            return  null;
        }

    }

    public TimsFrame(Integer id, Integer nbrScans, Integer nbrPeaks, Integer scanMode, Integer msmsType, Integer maxIntensity, Integer summedIntensity, Double time) {
        this.m_id = id;
        this.m_nbrScans = nbrScans;
        this.m_nbrPeaks = nbrPeaks;
        this.m_scanMode = ScanMode.findByCode(scanMode);
        this.m_msmsType = MsMsType.findByCode(msmsType);
        this.m_maxIntensity = maxIntensity;
        this.m_summedIntensity = summedIntensity;
        this.m_time = time;
        m_isPasef = m_msmsType.equals(MsMsType.PASEF);
    }

    public boolean isPasef(){
        return m_isPasef;
    }

    public Integer getId() {
        return m_id;
    }

    public Integer getNbrScans() {
        return m_nbrScans;
    }

    public Integer getNbrPeaks() {
        return m_nbrPeaks;
    }

    public void clearSpectraData() {
        if(m_spectrumByScan != null)
            m_spectrumByScan.clear();
        if(m_singleSpectrum !=null)
            m_singleSpectrum = null;
        if(m_isPasef) {
            for (PasefMsMsData msmsData : m_pasefMsMsInfoByPrecursor.values()) {
                msmsData.resetSpectrumData();
            }
        }
        m_spectrumDataSet = false;
    }
    public void setMassIntensityByScan( Map<Integer, Map<Double, Float>> massIntByScan){
        List<Integer> scansIndex = new ArrayList<>( massIntByScan.keySet());
        if(m_spectrumByScan == null)
            m_spectrumByScan = new HashMap<>();
        for(Integer scId : scansIndex) {
            Map<Double, Float> massInt = massIntByScan.get(scId);
            double[] scanMasses = new double[massInt.size()];
            float[] scanIntensities = new float[massInt.size()];
            final AtomicInteger index = new AtomicInteger();
            massInt.forEach((k, v) -> {
                scanMasses[index.get()] = k;
                scanIntensities[index.getAndIncrement()] = v;
            });

            if(m_isPasef) {  //VDS : To see for MsMs Data !
                for (PasefMsMsData msmsData : m_pasefMsMsInfoByPrecursor.values()) {
                    if (msmsData.containsScan(scId)) {
                        //Found PasefMsMs for current scan
                        msmsData.addSpectrumData(scanMasses, scanIntensities);
                    }
                }
            } else {
                m_spectrumByScan.put(scId, new Spectrum("Frame_"+m_id+"-scan_"+scId,1,getTime().floatValue(), scanMasses, scanIntensities));
            }
        }
        m_spectrumDataSet = true;
    }

    public List<Integer> getPrecursorId() {
        if(m_pasefMsMsInfoByPrecursor != null)
            return new ArrayList<>(m_pasefMsMsInfoByPrecursor.keySet());
        return null;
    }

    public Spectrum getPrecursorSpectrum( Integer precursorIndex) {
        if(m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
            return m_pasefMsMsInfoByPrecursor.get(precursorIndex).getPasefSpectrum();
        else
            return null;
    }

    public double getPrecursorCollisionEnergy(Integer precursorIndex){
        if(m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
            return m_pasefMsMsInfoByPrecursor.get(precursorIndex).getCollisionEnergy();
        else
            return 0.0;
    }

    public boolean spectrumRead(){
        return  m_spectrumDataSet;
    }

    public List<Spectrum> getAllSpectra(){
        if(!m_spectrumDataSet)
            return null;

        if(m_isPasef)
            return m_pasefMsMsInfoByPrecursor.values().stream().map(PasefMsMsData::getPasefSpectrum).collect(Collectors.toList());
        else
            return new ArrayList<>(m_spectrumByScan.values());
    }

    /**
     * Return how many Spectrum is defined for this FRAME. For PASEF Frame, PasefMsMsData should have been set first !
     * Return -1 if value is unknown
     * @return
     */
    public int getSpectrumCount(){
        if(!m_isPasef)
            return 1;
        else if(m_pasefMsMsInfoByPrecursor!=null)
            return m_pasefMsMsInfoByPrecursor.size();
        else
            return -1;
    }

    public Spectrum getSingleSpectrum(){
        if(!m_spectrumDataSet)
            return null;

        if(m_singleSpectrum ==null) {
            List<Spectrum> allSp = getAllSpectra();
            //VDS TODO If list allSp empty: create empty unique Spectra or return null ?
            Map<Double, Float> retainedMasses2Intensity = new HashMap<>();
            //   int nbrPeak = 0;
            for (Spectrum sp : allSp) {
                double[] spMasses = sp.getMasses();
                float[] spInstensities = sp.getIntensities();
                //  nbrPeak += spMasses.length;
                for (int i = 0; i < spMasses.length; i++) {
                    double nextMass =spMasses[i];
                    Float currentIntensity = retainedMasses2Intensity.getOrDefault(nextMass, 0f);
                    Float newIntensity = spInstensities[i];
                    if(currentIntensity.compareTo(newIntensity) <0)
                        retainedMasses2Intensity.put(nextMass, newIntensity);
                }
            }
            int msLevel = m_msmsType.equals(MsMsType.MS) ? 1 : 2;
            // LOG.trace("Frame_" + m_id+" has "+nbrPeak+" peaks, reduced to "+retainedMasses2Intensity.size());
            m_singleSpectrum = new Spectrum("Frame_" + m_id, msLevel, m_time.floatValue(), retainedMasses2Intensity);
        }

        return m_singleSpectrum;
    }

    public List<PasefMsMsData> getPasefMsMSData() {
        if(m_isPasef)
            return new ArrayList<>(m_pasefMsMsInfoByPrecursor.values());
        else
            return null;
    }

    public void setPasefMsMsData(List<PasefMsMsData> pasefMsMsInfo ) {
       pasefMsMsInfo.forEach( pMsms -> m_pasefMsMsInfoByPrecursor.put(pMsms.getPrecursorId(), pMsms));
    }

    public ScanMode getScanMode(){
        return m_scanMode;
    }

    public MsMsType getMsmsType() {
        return m_msmsType;
    }

    public Double getTime() {
        return m_time;
    }

    public Integer getMaxIntensity() {
        return m_maxIntensity;
    }

    public Integer getSummedIntensity() {
        return m_summedIntensity;
    }
}
