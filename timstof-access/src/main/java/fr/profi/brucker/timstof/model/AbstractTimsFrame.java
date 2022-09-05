package fr.profi.brucker.timstof.model;

import fr.profi.brucker.timstof.converter.SpectrumGeneratingMethod;
import it.unimi.dsi.fastutil.doubles.Double2FloatMap;
import it.unimi.dsi.fastutil.doubles.Double2FloatOpenHashMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/* TODO Create sub classes for PASEF Frame / MS Frame etc instead of MsmsType test in code ::
*
 */
public abstract class AbstractTimsFrame implements Comparable<AbstractTimsFrame> {
    protected final static Logger LOG = LoggerFactory.getLogger(AbstractTimsFrame.class);
    protected int m_id;
    protected int m_nbrScans;
    protected int m_nbrPeaks;

    /**
    --   0 = MS
    --   1 = AutoMSMS
    --   2 = MRM
    --   3 = in-source CID
    --   4 = broadband CID
    --   8 = PASEF
    --   9 = DIA
    */
    protected ScanMode m_scanMode;

    /**
    --   0 = MS frame
    --   2 = MS/MS fragment frame
    --   8 = PASEF frame
    --   9 = DIA frame
    */
    private MsMsType m_msmsType;

//    private boolean m_isPasef;
    /**
     * Time (in seconds), relative to the start time of the acquisition.
     */
    protected double m_time;

    /**
     * Maximum intensity occurring in all data belonging to this frame (can quickly generate a BPC from this)
     */
    protected int m_maxIntensity;

    /**
     * Sum of all intensities occurring in the data belonging to this frame (can quickly generate a TIC from this)
     */
    protected int m_summedIntensity;


    protected boolean m_spectrumDataSet =false;
    protected Spectrum m_singleSpectrum = null;

    /* a negative integer, zero, or a positive integer as this object
     * is less than, equal to, or greater than the specified object.
     */
    @Override
    public int compareTo(AbstractTimsFrame o) {
        if(o==null)
            return 1;
        return (int) (this.getTime() - o.getTime());
    }



    static Int2ObjectMap<MsMsType> msmsTypeByCode = new Int2ObjectOpenHashMap<>();
    public  enum  MsMsType {
        MS(0),
        MSMS(2),
        PASEF(8),
        DIA(9);

        int m_msmsTypeCode;

        MsMsType(int msmsTypeCode){
            this.m_msmsTypeCode=msmsTypeCode;
            msmsTypeByCode.put(msmsTypeCode, this);
        }

        public Integer getMsMsTypeCode(){
            return this.m_msmsTypeCode;
        }

        public static MsMsType findByCode(int code){
            return msmsTypeByCode.get(code);
        }

    }

    static Int2ObjectMap<ScanMode> scanModesByCode = new Int2ObjectOpenHashMap<>();
    public  enum  ScanMode {
        MS(0),
        AutoMSMS(1),
        MRM(2),
        IN_SOURCE_CID(3),
        BROADBAND_CID(4),
        PASEF(8),
        DIA(9);

        int m_modeCode;

        ScanMode(int modeCode){
            this.m_modeCode=modeCode;
            scanModesByCode.put(modeCode, this);
        }

        public Integer getScanModeCode(){
            return this.m_modeCode;
        }

        public static ScanMode findByCode(int code){
            return AbstractTimsFrame.scanModesByCode.get(code);
        }

    }

    public AbstractTimsFrame(int id, int nbrScans, int nbrPeaks, int scanMode, MsMsType msmsType, int maxIntensity, int summedIntensity, double time) {
        this.m_id = id;
        this.m_nbrScans = nbrScans;
        this.m_nbrPeaks = nbrPeaks;
        this.m_scanMode = ScanMode.findByCode(scanMode);
        this.m_msmsType = msmsType;
        this.m_maxIntensity = maxIntensity;
        this.m_summedIntensity = summedIntensity;
        this.m_time = time;
    }

    public int getId() {
        return m_id;
    }

    public int getNbrScans() {
        return m_nbrScans;
    }

    public int getNbrPeaks() {
        return m_nbrPeaks;
    }

    public ScanMode getScanMode(){
        return m_scanMode;
    }

    public MsMsType getMsmsType() {
        return m_msmsType;
    }

    public double getTime() {
        return m_time;
    }

    public int getMaxIntensity() {
        return m_maxIntensity;
    }

    public int getSummedIntensity() {
        return m_summedIntensity;
    }

    public void clearSpectraData(){
        if(m_singleSpectrum !=null)
            m_singleSpectrum = null;
        m_spectrumDataSet = false;

    }

    public abstract ObjectList<Spectrum> getAllSpectra();

    protected abstract void createSingleSpectrum(SpectrumGeneratingMethod msCreateMethod);

    public abstract void setMassIntensityByScan(Int2ObjectMap<Double2FloatMap> massIntByScan);

//    public void setMassIntensityByScan( Map<Integer, Map<Double, Float>> massIntByScan){
//        List<Integer> scansIndex = new ArrayList<>( massIntByScan.keySet());
//        if(m_spectrumByScan == null)
//            m_spectrumByScan = new HashMap<>();
//        for(Integer scId : scansIndex) {
//            Map<Double, Float> massInt = massIntByScan.get(scId);
//            double[] scanMasses = new double[massInt.size()];
//            float[] scanIntensities = new float[massInt.size()];
//            final AtomicInteger index = new AtomicInteger();
//            massInt.forEach((k, v) -> {
//                scanMasses[index.get()] = k;
//                scanIntensities[index.getAndIncrement()] = v;
//            });
//
//            if(m_isPasef) {  //VDS : To see for MsMs Data !
//                for (PasefMsMsData msmsData : m_pasefMsMsInfoByPrecursor.values()) {
//                    if (msmsData.containsScan(scId)) {
//                        //Found PasefMsMs for current scan
//                        msmsData.addSpectrumData(scanMasses, scanIntensities);
//                    }
//                }
//            } else {
//                m_spectrumByScan.put(scId, new Spectrum("Frame_"+m_id+"-scan_"+scId,1,getTime().floatValue(), scanMasses, scanIntensities));
//            }
//        }
//        m_spectrumDataSet = true;
//    }


    public boolean spectrumRead(){
        return  m_spectrumDataSet;
    }
    /**
     * Return how many Spectrum is defined for this FRAME. For PASEF Frame, PasefMsMsData should have been set first !
     * Return -1 if value is unknown
     * @return
     */
    public int getSpectrumCount(){
        return -1;
    }

    protected void createSingleSpFromAllSpectra(){
        Double2FloatMap retainedMasses2Intensity = new Double2FloatOpenHashMap();
        ObjectList<Spectrum> allSp = getAllSpectra();
        //VDS TODO If list allSp empty: create empty unique Spectra or return null ?
//        int nbrPeak = 0;
        for (Spectrum sp : allSp) {
            double[] spMasses = sp.getMasses();
            float[] spInstensities = sp.getIntensities();
//            nbrPeak += spMasses.length;
            for (int i = 0; i < spMasses.length; i++) {
                double nextMass = spMasses[i];
                float currentIntensity = retainedMasses2Intensity.getOrDefault(nextMass, 0f);
                float newIntensity = spInstensities[i];
                if (currentIntensity < newIntensity)
                    retainedMasses2Intensity.put(nextMass, newIntensity);
            }
        }
//        LOG.debug("Frame_" + m_id + " has " + nbrPeak + " peaks, reduced to " + retainedMasses2Intensity.size());
        int msLevel = (m_msmsType.equals(MsMsType.MS)) ? 1 : 2;
        m_singleSpectrum = new Spectrum("Frame_" + m_id, msLevel, (float) m_time, retainedMasses2Intensity);
    }


    public Spectrum getSingleSpectrum(SpectrumGeneratingMethod msCreateMethod) {
        if(!m_spectrumDataSet)
            return null;

        if(m_singleSpectrum == null)
            createSingleSpectrum(msCreateMethod);

        return m_singleSpectrum;
    }


}
