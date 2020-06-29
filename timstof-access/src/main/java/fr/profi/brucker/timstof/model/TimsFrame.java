package fr.profi.brucker.timstof.model;

import it.unimi.dsi.fastutil.doubles.Double2FloatMap;
import it.unimi.dsi.fastutil.doubles.Double2FloatOpenHashMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntIterator;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import it.unimi.dsi.fastutil.objects.ObjectIterator;
import it.unimi.dsi.fastutil.objects.ObjectList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;


/* TODO Create sub classes for PASEF Frame / MS Frame etc instead of MsmsType test in code ::
*
 */
public class TimsFrame implements Comparable<TimsFrame> {
    private final static Logger LOG = LoggerFactory.getLogger(TimsFrame.class);
    private int m_id;
    private int m_nbrScans;
    private int m_nbrPeaks;

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
    private double m_time;

    /**
     * Maximum intensity occurring in all data belonging to this frame (can quickly generate a BPC from this)
     */
    private int m_maxIntensity;

    /**
     * Sum of all intensities occurring in the data belonging to this frame (can quickly generate a TIC from this)
     */
    private int m_summedIntensity;

   // private Map<Integer, Map<Double, Float>> m_massIntensityByScan;

    //PASEF Frame specific
    private Int2ObjectMap<PasefMsMsData> m_pasefMsMsInfoByPrecursor = new Int2ObjectOpenHashMap<>();

    //MS frame specific
    private Int2ObjectMap<Spectrum> m_spectrumByScan;
    private boolean m_spectrumDataSet =false;

    private Spectrum m_singleSpectrum = null;

    /* a negative integer, zero, or a positive integer as this object
     * is less than, equal to, or greater than the specified object.
     */
    @Override
    public int compareTo(TimsFrame o) {
        if(o==null)
            return 1;
        return (int) (this.getTime() - o.getTime());
    }



    static Int2ObjectMap<MsMsType> msmsTypeByCode = new Int2ObjectOpenHashMap();
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

    static Int2ObjectMap<ScanMode> scanModesByCode = new Int2ObjectOpenHashMap();
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
            return TimsFrame.scanModesByCode.get(code);
        }

    }

    public TimsFrame(int id, int nbrScans, int nbrPeaks, int scanMode, int msmsType, int maxIntensity, int summedIntensity, double time) {
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

    public int getId() {
        return m_id;
    }

    public int getNbrScans() {
        return m_nbrScans;
    }

    public int getNbrPeaks() {
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

    public void setMassIntensityByScan(Int2ObjectMap<Double2FloatMap> massIntByScan){
        if(m_spectrumByScan == null)
            m_spectrumByScan = new Int2ObjectOpenHashMap<>();
        IntIterator scansIdIt= massIntByScan.keySet().iterator();
        while (scansIdIt.hasNext()){
            int scId = scansIdIt.nextInt();
            Double2FloatMap massInt = massIntByScan.get(scId);

            if(m_isPasef) {  //VDS : To see for MsMs Data !
                for (PasefMsMsData msmsData : m_pasefMsMsInfoByPrecursor.values()) {
                    if (msmsData.containsScan(scId)) {
                        //Found PasefMsMs for current scan
                        msmsData.addSpectrumData(massInt);
                    }
                }
            } else {
                double[] scanMasses = new double[massInt.size()];
                float[] scanIntensities = new float[massInt.size()];
                ObjectIterator<Double2FloatMap.Entry> entries = massInt.double2FloatEntrySet().iterator();
                Double2FloatMap.Entry dataEntry;
                int index = 0;
                while (entries.hasNext()){
                    dataEntry = entries.next();
                    double massVal = dataEntry.getDoubleKey();
                    float intensityVal = dataEntry.getFloatValue();
                    scanMasses[index] = massVal;
                    scanIntensities[index++] = intensityVal;
                }
                m_spectrumByScan.put(scId, new Spectrum("Frame_"+m_id+"-scan_"+scId,1, (float)getTime(), scanMasses, scanIntensities));
            }
        }
        m_spectrumDataSet = true;
    }

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

    public IntArrayList getPrecursorIds() {
        if(m_pasefMsMsInfoByPrecursor != null)
            return new IntArrayList(m_pasefMsMsInfoByPrecursor.keySet());
        return null;
    }

    public Spectrum getPrecursorSpectrum( int precursorIndex) {
        if(m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
            return m_pasefMsMsInfoByPrecursor.get(precursorIndex).getPasefSpectrum();
        else
            return null;
    }

    public double getPrecursorCollisionEnergy(int precursorIndex){
        if(m_pasefMsMsInfoByPrecursor != null && m_spectrumDataSet)
            return m_pasefMsMsInfoByPrecursor.get(precursorIndex).getCollisionEnergy();
        else
            return 0.0;
    }

    public boolean spectrumRead(){
        return  m_spectrumDataSet;
    }

    public ObjectList<Spectrum> getAllSpectra(){
        if(!m_spectrumDataSet)
            return null;

        if(m_isPasef) {
            ObjectList<Spectrum> allSp = new ObjectArrayList<>();
            ObjectIterator<PasefMsMsData> pasefMsMsDataIt = m_pasefMsMsInfoByPrecursor.values().iterator();
            while (pasefMsMsDataIt.hasNext()) {
                allSp.add(pasefMsMsDataIt.next().getPasefSpectrum());
            }
            return allSp;
        } else
            return new ObjectArrayList<>(m_spectrumByScan.values());
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
            ObjectList<Spectrum> allSp = getAllSpectra();
            //VDS TODO If list allSp empty: create empty unique Spectra or return null ?
            Double2FloatMap retainedMasses2Intensity = new Double2FloatOpenHashMap();
               int nbrPeak = 0;
            for (Spectrum sp : allSp) {
                double[] spMasses = sp.getMasses();
                float[] spInstensities = sp.getIntensities();
              nbrPeak += spMasses.length;
                for (int i = 0; i < spMasses.length; i++) {
                    double nextMass =spMasses[i];
                    float currentIntensity = retainedMasses2Intensity.getOrDefault(nextMass, 0f);
                    float newIntensity = spInstensities[i];
                    if(currentIntensity < newIntensity)
                        retainedMasses2Intensity.put(nextMass, newIntensity);
                }
            }
            int msLevel = m_msmsType.equals(MsMsType.MS) ? 1 : 2;
            LOG.trace("Frame_" + m_id+" has "+nbrPeak+" peaks, reduced to "+retainedMasses2Intensity.size());
            m_singleSpectrum = new Spectrum("Frame_" + m_id, msLevel, (float)m_time, retainedMasses2Intensity);
        }

        return m_singleSpectrum;
    }

    public List<PasefMsMsData> getPasefMsMSData() {
        if(m_isPasef)
            return new ObjectArrayList<>(m_pasefMsMsInfoByPrecursor.values());
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

    public double getTime() {
        return m_time;
    }

    public int getMaxIntensity() {
        return m_maxIntensity;
    }

    public int getSummedIntensity() {
        return m_summedIntensity;
    }
}
