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


    public  void fillFramesWithSpectrumData(Long fileHandle, List<TimsFrame> frames){
        checkFileHandle(fileHandle);
        //--- VDS TODO check or Force fillFramesWithMsMsInfo
        //-> VDS For timing logs
//        long time1 =0;
//        long time2 =0;
//        long time3 =0;

        // --- read scans/msms data
        for(TimsFrame frame : frames){
//            long start = System.currentTimeMillis();//-> VDS For timing logs
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
            //LOG.trace("reading frame {}, buffer length: {}", frameId, buf_len);

            IntBuffer intBuf = ByteBuffer.wrap(buffer).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer();
            int[] scanBuffer = new int[intBuf.remaining()];
            intBuf.get(scanBuffer);
            //-> VDS For timing logs
//            long step1 = System.currentTimeMillis();
//            time1 += step1-start;

            // check out the layout of scanBuffer:
            // - the first numScan integers specify the number of peaks for each scan
            // - the next integers are pairs of (x,y) values for the scans. The x values are not masses but index values
            Map<Integer, Map<Integer,Float>> scanIndices2IntensityMap = new HashMap<>();
            List<Integer> indicesToSearch  = new ArrayList<>();
            long error_stat = 0;
            int d = nbrScans;
            for (int i = 0; i < nbrScans; ++i) {

                int numberPeaks = scanBuffer[i];
                if(numberPeaks != 0) {
                    int[] indices = Arrays.copyOfRange(scanBuffer, d, d + numberPeaks);
                    d += numberPeaks;
                    int[] scanIntensities = Arrays.copyOfRange(scanBuffer, d, d + numberPeaks);
                    d += numberPeaks;

                    // you are not really interested in index values for x-axis but masses
//                    long s1 = System.currentTimeMillis();
//                    double[] scanMasses = new double[indices.length];
//                    error_stat = m_tdfLib.tims_index_to_mz(fileHandle, frameId, ArraysUtil.copyFromIntArray(indices), scanMasses, scanMasses.length);
//                    for(int inde = 0; inde<scanMasses.length; inde++) {
//                        LOG.debug("\t" +indices[inde]+"\t"+scanMasses[inde]);
//                    }
//                    if (0 == error_stat) {
//                        LOG.error(" !!! could not convert indices to masses for frame {} scan {}.", frameId, i);
//                    }
                    //-> VDS For timing logs
//                    long s2 = System.currentTimeMillis();
//                    time3 += s2-s1;
//                    System.out.printf("scan %d has %d peaks", i, numberPeaks).println();
//                    System.out.println(Arrays.toString(scanMasses));
//                    System.out.println(Arrays.toString(scanIntensities));
//                    Map<Double, Float> massInstensityMap = new HashMap<>();
                    Map<Integer, Float> indiceInstensityMap = new HashMap<>();
                    for(int peakIndex =0; peakIndex < numberPeaks; peakIndex++){
                        indicesToSearch.add(indices[peakIndex]);
//                        massInstensityMap.put(scanMasses[peakIndex], new Float(String.valueOf(scanIntensities[peakIndex])) );
                        indiceInstensityMap.put(indices[peakIndex],new Float(String.valueOf(scanIntensities[peakIndex])) );
                    }
                    scanIndices2IntensityMap.put(i, indiceInstensityMap);
                } //End at least 1 peak in scan
            } // End for all scans

            //-> VDS For timing logs
//            long step2 = System.currentTimeMillis();
//            time2 += step2- step1;
            //LOG.trace( "Read {} peaks for frame {} ",totalNbrPeaks, frameId);

            //convert indices to masses and create spectra data
            double[] scanMasses = new double[indicesToSearch.size()];
            error_stat = m_tdfLib.tims_index_to_mz(fileHandle, frameId, ArraysUtil.copyFromIntList(indicesToSearch), scanMasses, scanMasses.length);
            if (0 == error_stat) {
                LOG.error(" !!! could not convert indices to masses for frame {}.", frameId);
            }
            //Create Indice to Mass Map
            Map<Integer,Double> indiceToMassMap = new HashMap<>();
            for(int indiceIndex =0; indiceIndex < scanMasses.length; indiceIndex++)
                indiceToMassMap.put(indicesToSearch.get(indiceIndex), scanMasses[indiceIndex]);

            //Convert indice,intensity tuple to mass,intensity
            Map<Integer, Map<Double,Float>> scanMsMsDataMap = new HashMap<>();
            for(Map.Entry<Integer, Map<Integer, Float>> entry: scanIndices2IntensityMap.entrySet()){
                Map<Integer, Float> indiceToIntensity = entry.getValue();
                Map<Double,Float> massIntentisyMap = new HashMap<>();
                for(Map.Entry<Integer,Float> nextIndiceToIntensity : indiceToIntensity.entrySet())
                    massIntentisyMap.put(indiceToMassMap.get(nextIndiceToIntensity.getKey()), nextIndiceToIntensity.getValue());
                scanMsMsDataMap.put(entry.getKey(),massIntentisyMap);
            }
//            long end = System.currentTimeMillis();
//            time3 += end- step2;
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

        }//End for all Frames
//        LOG.debug(" TIME for read Frame. read scans "+time1+ "; go throw scans  "+time2+" to get masses from index "+time3);
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
