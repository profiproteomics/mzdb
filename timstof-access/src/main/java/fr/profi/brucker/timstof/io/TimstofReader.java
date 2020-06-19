package fr.profi.brucker.timstof.io;


import fr.profi.brucker.timstof.TDFLibrary;
import fr.profi.brucker.timstof.TDFNativeLibrariesFactory;
import fr.profi.brucker.timstof.model.Precursor;
import fr.profi.brucker.timstof.model.TimsFrame;
import fr.profi.brucker.timstof.util.ArraysUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class TimstofReader {

    private final static Logger LOG = LoggerFactory.getLogger(TimstofReader.class);
    private static TDFLibrary m_tdfLib;
    private static TimstofReader m_instance = null;

    private static HashMap<Long,File> m_ttFilesByHandle;
    private static HashMap<Long, TDFMetadataReader> m_cachedMetaReaderByHandle;

    public static TimstofReader getTimstofReader() {
        if(m_instance == null) {
            m_tdfLib = TDFNativeLibrariesFactory.loadAndGetNativeLibraries();
            m_instance = new TimstofReader();
            m_ttFilesByHandle = new HashMap<>();
            m_cachedMetaReaderByHandle= new HashMap<>();
        }
        return m_instance;
    }

    private TimstofReader() {
    }

    public Long openTimstofFile(File f) throws IllegalArgumentException{
        long handle = m_tdfLib.tims_open(f.getAbsolutePath(), 0);
        LOG.info(" Open file handle " + handle);
        if (handle == 0) {
            byte[] errorBuffer = new byte[64];
            long len = m_tdfLib.tims_get_last_error_string(errorBuffer, errorBuffer.length);
            StringBuffer errMsg = new StringBuffer(new String(errorBuffer, StandardCharsets.UTF_8));
            if (len > 64)
                errMsg.append("...");
            LOG.error("TimsToff errorBuffer " + errMsg.toString());
            throw new IllegalArgumentException(errMsg.toString());
        } else {
            m_ttFilesByHandle.put(handle, f);
            return handle;
        }
    }

    public void closeTimstofFile(Long fileHandle){
        m_ttFilesByHandle.remove(fileHandle);
        m_cachedMetaReaderByHandle.remove(fileHandle);
        m_tdfLib.tims_close(fileHandle);
    }

    private void checkFileHandle(Long fileHandle){
        if (!m_ttFilesByHandle.containsKey(fileHandle))
            throw new IllegalArgumentException(" No Timstof file associated to handle " + fileHandle);
    }

    /**
     * This method will read and create TimsFrame as well as it will
     * fill these TimsFrame with MSMS Frame Info. It is equivalent to
     * call getTimsFrames then fillFramesWithMsMsInfo
     * @param fileHandle
     * @return
     */
    public List<TimsFrame> getFullTimsFrames(Long fileHandle) {
        List<TimsFrame> timsFrames = getTimsFrames(fileHandle);
        fillFramesWithMsMsInfo(fileHandle,timsFrames);
        return  timsFrames;
   }

    public List<TimsFrame> getTimsFrames(Long fileHandle) {
        checkFileHandle(fileHandle);

        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);

        Integer nbrFrames = metaDataReader.getFrameCount();
        List<Integer> framesIds = new ArrayList<>();
        for (int i = 1; i <= nbrFrames; i++) {
            framesIds.add(i);
        }
        // --- read metaData information
        return metaDataReader.readFramesInfo(framesIds);

    }

    /**
     * Only implemented for PASEF MsMsInfo
     * @param fileHandle: Timstof analysis file
     * @param frames: List of frame to get info for
     * @return filled frames
     */
    public void fillFramesWithMsMsInfo(Long fileHandle, List<TimsFrame> frames){
        checkFileHandle(fileHandle);

        // --- read msms info
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));

        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);
        metaDataReader.readPasefMsMsInfo(frames);
    }

    //-> VDS-TIME: For timing logs
    public static long time_readScans = 0;
    public static long time_extractPeaks =0;
    public static long time_indiceToMass =0;
    public static long time_indiceToMassStep2 =0;

    public static long time_indiceToMassMap =0;
    public static long time_indiceToMassMapS1 =0;
    public static long time_indiceToMassMapS2 =0;
    public static long time_indiceToMassMapS3 =0;
    public static int nbrRead =0;

    public  void fillFramesWithSpectrumData(Long fileHandle, List<TimsFrame> frames){
        checkFileHandle(fileHandle);
        //--- VDS TODO check or Force fillFramesWithMsMsInfo

        // --- read scans/msms data
        for(TimsFrame frame : frames){
            //--> VDS-TIME: For timing logs
            nbrRead++;
            long start = System.currentTimeMillis();

            long frameId = frame.getId();
            int nbrScans = frame.getNbrScans();

            int inputBufferLen = (frame.isPasef()) ?  200000 : 3000000; //OK for PASEF frames
            byte[] buffer = new byte[inputBufferLen];
            long buf_len = m_tdfLib.tims_read_scans_v2(fileHandle, frameId, 0, nbrScans, buffer, inputBufferLen);
            if (buf_len == 0) {
                LOG.error(" !!!! Error reading scans for frame %d"+frameId);
            }
            if(buf_len > inputBufferLen){
                LOG.trace(" !!!! Error: missing data "+buf_len);
                int newSize = (int) buf_len+10;
                buffer = new byte[newSize];
                m_tdfLib.tims_read_scans_v2(fileHandle, frameId, 0, nbrScans, buffer, newSize);
            }

            IntBuffer intBuf = ByteBuffer.wrap(buffer).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer();
            int[] scanBuffer = new int[intBuf.remaining()];
            intBuf.get(scanBuffer);
            //-> VDS-TIME: For timing logs
            long step1 = System.currentTimeMillis();
            time_readScans += step1-start; //--> VDS-TIME: Ecoli (10Go).  ~4.5min

            // check out the layout of scanBuffer:
            // - the first numScan integers specify the number of peaks for each scan
            // - the next integers are pairs of (x,y) values for the scans. The x values are not masses but index values
            Map<Integer, Map<Integer,Float>> scanIndices2IntensityMap = new HashMap<>();
            List<Integer> indicesToSearch  = new ArrayList<>();
            long error_stat = 0;
            int d = nbrScans;
//            int totalNbrPeaks =0;
//            int totalNbrScans =0;
            for (int i = 0; i < nbrScans; ++i) {

                int numberPeaks = scanBuffer[i];
                if(numberPeaks != 0) {
                    int[] indices = Arrays.copyOfRange(scanBuffer, d, d + numberPeaks);
                    d += numberPeaks;
                    int[] scanIntensities = Arrays.copyOfRange(scanBuffer, d, d + numberPeaks);
                    d += numberPeaks;
//                    totalNbrPeaks += numberPeaks;
//                    totalNbrScans += 1;
                    Map<Integer, Float> indiceInstensityMap = new HashMap<>();
                    for(int peakIndex =0; peakIndex < numberPeaks; peakIndex++){
                        indicesToSearch.add(indices[peakIndex]);
                        indiceInstensityMap.put(indices[peakIndex],new Float(String.valueOf(scanIntensities[peakIndex])) );
                    }
                    scanIndices2IntensityMap.put(i, indiceInstensityMap);
                } //End at least 1 peak in scan
            } // End for all scans

            //-> VDS For timing logs
            long step2 = System.currentTimeMillis();
            time_extractPeaks += step2-step1; //--> VDS-TIME: Ecoli (10Go)   In "Timstof2MzDB" entre 5 et 6min
//            LOG.debug( "\tRead frame:\t{}\tnbr Scans:\t{}\tnbr peaks:\t{}", frameId, totalNbrScans, totalNbrPeaks);

            //convert indices to masses and create spectra data
            double[] scanMasses =  new double[indicesToSearch.size()];
            error_stat = m_tdfLib.tims_index_to_mz(fileHandle, frameId, ArraysUtil.convertToDoubleArray(indicesToSearch), scanMasses, scanMasses.length);
            if (0 == error_stat) {
                LOG.error(" !!! could not convert indices to masses for frame {}.", frameId);
            }

            //-> VDS For timing logs
            long step21 = System.currentTimeMillis();
            time_indiceToMass += step21-step2; //--> VDS-TIME: Ecoli (10Go) ~1min

            //Create Indice to Mass Map
            Map<Integer,Double> indiceToMassMap = new HashMap<>();
            for(int indiceIndex =0; indiceIndex < scanMasses.length; indiceIndex++)
                indiceToMassMap.put(indicesToSearch.get(indiceIndex), scanMasses[indiceIndex]);

            //Convert indice,intensity tuple to mass,intensity
            Map<Integer, Map<Double,Float>> scanMsMsDataMap = new HashMap<>();
            //-> VDS For timing logs
            long step22 = System.currentTimeMillis();
            time_indiceToMassMapS1 += step22-step21; //--> VDS-TIME: Ecoli (10Go) ~4-5min

            for(Map.Entry<Integer, Map<Integer, Float>> entry: scanIndices2IntensityMap.entrySet()){
                long startWh = System.currentTimeMillis();
                Integer scanId = entry.getKey();
                Map<Integer, Float> indiceToIntensity = entry.getValue();
                Map<Double,Float> massIntentisyMap = new HashMap<>();
                long step23 = System.currentTimeMillis();
                time_indiceToMassMapS2 += step23-startWh;//--> VDS-TIME: Ecoli (10Go) <1s

                for(Map.Entry<Integer, Float> e : indiceToIntensity.entrySet()){
                    Integer nextIndice =  e.getKey();
                    massIntentisyMap.put(indiceToMassMap.get(nextIndice), e.getValue());
                }
                long step24 = System.currentTimeMillis();
                time_indiceToMassMapS3 += step24-step23;//--> VDS-TIME: Ecoli (10Go) ~6-7mins
                scanMsMsDataMap.put(scanId,massIntentisyMap);
            }
            long end = System.currentTimeMillis();
            //--> VDS-TIME:  Ecoli (10Go). FROM step2 In "Timstof2MzDB" ~15.2min // = time_indiceToMassMapS2 +  time_indiceToMassMapS3 + scanMsMsDataMap.put: ~ 6-7min
            time_indiceToMassMap += end- step22;
            frame.setMassIntensityByScan(scanMsMsDataMap);

            //VDS: test to get IonMobility Value
//            double[] scanAsDbl = new double [scanMsMsDataMap.size()];
//            double[] ionMobilities = new double [scanMsMsDataMap.size()];
//            List<Integer> keys = scanMsMsDataMap.keySet().stream().collect(Collectors.toList());
//            for(int i =0 ; i<scanMsMsDataMap.size(); i++){
//                scanAsDbl[i] = keys.get(i);
//            }
//
//            error_stat = m_tdfLib.tims_scannum_to_oneoverk0(fileHandle, frameId, scanAsDbl,ionMobilities, ionMobilities.length);
//            if (0 == error_stat) {
//                LOG.error(" !!! could not convert scans to ion mobility for frame {} scan {}.", frameId);
//            } else{
//                for(int i =0 ; i<scanMsMsDataMap.size(); i++){
//
//                    System.out.println(" Scans "+scanAsDbl[i]+" => Ion Mobility "+ionMobilities[i]);
//
//                }
//            }
            //-> VDS For timing logs
            if(nbrRead % 100 == 0){
                LOG.debug(" TIME reading "+nbrRead+" frame(s):\tScans data read\t"+time_readScans+ "\tExtract peaks from buffer \t"+time_extractPeaks+"\tGet masses from indexes\t"+time_indiceToMass+"\tmap from indexes to mass\t"+time_indiceToMassMapS1+" - "+time_indiceToMassMapS2+" - "+time_indiceToMassMapS3+" - "+time_indiceToMassMap);
                time_readScans = 0;
                time_extractPeaks = 0;
                time_indiceToMass=0;
                time_indiceToMassMap=0;
                time_indiceToMassMapS1=0;
                time_indiceToMassMapS2=0;
                time_indiceToMassMapS3=0;
            }
        }   //End for all Frames
    }

    public TDFLibrary getTDFLib() {
        return m_tdfLib;
    }

    public List<Precursor> readPrecursorInfo(Long fileHandle){
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);

        return metaDataReader.readPrecursorInfo();
    }

    public Map<String, String> readGlobalProperties(Long fileHandle){
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);
        return metaDataReader.readGlobalMetaData();

    }
 }
