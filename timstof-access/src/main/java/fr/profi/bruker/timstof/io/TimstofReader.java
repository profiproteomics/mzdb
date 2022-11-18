package fr.profi.bruker.timstof.io;


import fr.profi.bruker.timstof.TDFLibrary;
import fr.profi.bruker.timstof.TDFNativeLibrariesFactory;
import fr.profi.bruker.timstof.model.AbstractTimsFrame;
import fr.profi.bruker.timstof.model.Precursor;
import fr.profi.bruker.timstof.model.TimsPASEFFrame;
import fr.profi.bruker.timstof.util.ArraysUtil;
import it.unimi.dsi.fastutil.doubles.Double2FloatMap;
import it.unimi.dsi.fastutil.doubles.Double2FloatOpenHashMap;
import it.unimi.dsi.fastutil.ints.*;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.objects.ObjectIterator;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TimstofReader {

    private final static Logger LOG = LoggerFactory.getLogger(TimstofReader.class);
    private static TDFLibrary m_tdfLib;
    private static TimstofReader m_instance = null;

    //VDS: keep these map or suppose only one file will be read...
    private static Long2ObjectMap<File> m_ttFilesByHandle;
    private static Long2ObjectMap<TDFMetadataReader> m_cachedMetaReaderByHandle;

    public static TimstofReader getTimstofReader() {
        if(m_instance == null) {
            m_tdfLib = TDFNativeLibrariesFactory.loadAndGetNativeLibraries();
            m_instance = new TimstofReader();
            m_ttFilesByHandle = new Long2ObjectOpenHashMap<>();
            m_cachedMetaReaderByHandle= new Long2ObjectOpenHashMap<>();
        }
        return m_instance;
    }

    private TimstofReader() {
    }

    public Long openTimstofFile(File f) throws IllegalArgumentException{
        long handle = m_tdfLib.tims_open(f.getAbsolutePath(), 0);
        LOG.info(" Open file "+ f.getAbsolutePath()+ ", handle= " + handle);
        if (handle == 0) {
            byte[] errorBuffer = new byte[64];
            long len = m_tdfLib.tims_get_last_error_string(errorBuffer, errorBuffer.length);
            StringBuilder errMsg = new StringBuilder(new String(errorBuffer, StandardCharsets.UTF_8));
            if (len > 64)
                errMsg.append("...");
            LOG.error("TimsToff errorBuffer " + errMsg.toString());
            throw new IllegalArgumentException(errMsg.toString());
        } else {
            m_ttFilesByHandle.put(handle, f);
            return handle;
        }
    }

    public void closeTimstofFile(long fileHandle){
        m_ttFilesByHandle.remove(fileHandle);
        m_cachedMetaReaderByHandle.remove(fileHandle);
        m_tdfLib.tims_close(fileHandle);
    }

    private void checkFileHandle(long fileHandle){
        if (!m_ttFilesByHandle.containsKey(fileHandle))
            throw new IllegalArgumentException(" No Timstof file associated to handle " + fileHandle);
    }

    /**
     * This method will read and create TimsFrame as well as it will
     * fill these TimsFrame with MSMS Frame Info. It is equivalent to
     * call getTimsFrames then fillFramesWithMsMsInfo
     * @param fileHandle handle to identify TimsTof file to read from
     * @return List of frame in specified file with associated MSMS info
     */
    public List<AbstractTimsFrame> getFullTimsFrames(long fileHandle) {
        List<AbstractTimsFrame> timsFrames = getTimsFrames(fileHandle);
        List<TimsPASEFFrame> pasefFrames = new ArrayList<>();
        timsFrames.forEach( f -> {
            if(f.getMsmsType().equals(AbstractTimsFrame.MsMsType.PASEF))
                pasefFrames.add((TimsPASEFFrame)f);
        });
        fillFramesWithMsMsInfo(fileHandle,pasefFrames);
        return  timsFrames;
   }

    public List<AbstractTimsFrame> getTimsFrames(long fileHandle) {
        checkFileHandle(fileHandle);

        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader = m_cachedMetaReaderByHandle.get(fileHandle);

        int nbrFrames = metaDataReader.getFrameCount();
        IntArrayList framesIds = new IntArrayList(nbrFrames*4/3+1);
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
     */
    public void fillFramesWithMsMsInfo(long fileHandle, List<TimsPASEFFrame> frames){
        checkFileHandle(fileHandle);

        // --- read msms info
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));

        TDFMetadataReader metaDataReader = m_cachedMetaReaderByHandle.get(fileHandle);
        metaDataReader.readPasefMsMsInfo(frames);
    }

    //-> VDS-TIME: For timing logs
//    public static long time_readScans = 0;
//    public static long time_extractPeaks =0;
//    public static long time_indiceToMass =0;
//    public static long time_indiceToMassMap =0;
//    public static long time_indiceToMassMapS1 =0;
//    public static long time_indiceToMassMapS2 =0;
//    public static long time_indiceToMassMapS3 =0;
//    public static int nbrRead =0;

    public  void fillFramesWithSpectrumData(Long fileHandle, List<? extends AbstractTimsFrame> frames){
        checkFileHandle(fileHandle);
        //--- VDS TODO check or Force fillFramesWithMsMsInfo

        // --- read scans/msms data
        for(AbstractTimsFrame frame : frames){
            //--> VDS-TIME: For timing logs
//            nbrRead++;
//            long start = System.currentTimeMillis();

            long frameId = frame.getId();
            int nbrScans = frame.getNbrScans();

            int inputBufferLen = (frame.getMsmsType().equals(AbstractTimsFrame.MsMsType.PASEF)) ?  200000 : 3000000; //OK for PASEF frames
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
//            long step1 = System.currentTimeMillis();
//            time_readScans += step1-start; //--> VDS-TIME: Ecoli (10Go).  ~4.5min

            // check out the layout of scanBuffer:
            // - the first numScan integers specify the number of peaks for each scan
            // - the next integers are pairs of (x,y) values for the scans. The x values are not masses but index values
            Int2ObjectMap<Int2FloatMap> scanIndices2IntensityMap = new Int2ObjectOpenHashMap<>(nbrScans*4/3+1);
            int[] indicesToSearch;
            long error_stat;
            int d = nbrScans;
//            int totalNbrPeaks =0;
//            int totalNbrScans =0;
            int nbPeaksTotal = 0;
            for (int i = 0; i < nbrScans; ++i) {
                nbPeaksTotal += scanBuffer[i];
            }
            indicesToSearch = new int[nbPeaksTotal];
            int indicesToSearchIndex = 0;


            for (int i = 0; i < nbrScans; ++i) {

                int numberPeaks = scanBuffer[i];
                if(numberPeaks != 0) {

                    int startIndices = d;
                    d += numberPeaks;

                    int startScanIntensities = d;
                    d += numberPeaks;

                    Int2FloatMap indiceIntensityMap = new Int2FloatOpenHashMap(numberPeaks*4/3+1);
                    for(int peakIndex = 0; peakIndex < numberPeaks; peakIndex++){
                        int indix = scanBuffer[startIndices+peakIndex];
                        indicesToSearch[indicesToSearchIndex++] = indix;
                        indiceIntensityMap.put(indix, (float)scanBuffer[startScanIntensities+peakIndex]);
                    }

                    scanIndices2IntensityMap.put(i, indiceIntensityMap);
                } //End at least 1 peak in scan
            } // End for all scans

            //-> VDS For timing logs
//            long step2 = System.currentTimeMillis();
//            time_extractPeaks += step2-step1; //--> VDS-TIME: Ecoli (10Go)   In "Timstof2MzDB" entre 5 et 6min

//            LOG.debug( "\tRead frame:\t{}\tnbr Scans:\t{}\tnbr peaks:\t{}", frameId, totalNbrScans, totalNbrPeaks);

            //convert indices to masses and create spectra data
            double[] scanMasses =  new double[nbPeaksTotal];
            error_stat = m_tdfLib.tims_index_to_mz(fileHandle, frameId, ArraysUtil.copyFromIntArray(indicesToSearch), scanMasses, scanMasses.length);
            if (0 == error_stat) {
                LOG.error(" !!! could not convert indices to masses for frame {}.", frameId);
            }

            //-> VDS For timing logs
//            long step21 = System.currentTimeMillis();
//            time_indiceToMass += step21-step2; //--> VDS-TIME: Ecoli (10Go) ~1min

          //Create Indice to Mass Map
          Int2DoubleMap indiceToMassMap = new Int2DoubleOpenHashMap(nbPeaksTotal*4/3+1);
          for(int indiceIndex = 0; indiceIndex < scanMasses.length; indiceIndex++)
            indiceToMassMap.put(indicesToSearch[indiceIndex], scanMasses[indiceIndex]);

          //Convert indice,intensity tuple to mass,intensity
          Int2ObjectMap<Double2FloatMap> scanMsMsDataMap = new Int2ObjectOpenHashMap<>(nbrScans * 4 / 3 + 1);
          //-> VDS For timing logs
//          long step22 = System.currentTimeMillis();
//          time_indiceToMassMapS1 += step22-step21; //--> VDS-TIME: Ecoli (10Go) ~4-5min

          ObjectIterator<Int2ObjectMap.Entry<Int2FloatMap>> scansIter = scanIndices2IntensityMap.int2ObjectEntrySet().iterator();
          Int2ObjectMap.Entry<Int2FloatMap> scansEntry;
          while (scansIter.hasNext()) {
//              long startWh = System.currentTimeMillis();
              scansEntry = scansIter.next();
              int scanId = scansEntry.getIntKey();
              Int2FloatMap indiceToIntensity = scansEntry.getValue();
              Double2FloatMap massIntensityMap = new Double2FloatOpenHashMap(indiceToIntensity.size()*4/3+1);
//              long step23 = System.currentTimeMillis();
//              time_indiceToMassMapS2 += step23-startWh;//--> VDS-TIME: Ecoli (10Go) <1s

              ObjectIterator<Int2FloatMap.Entry> indiceToIntensityIt = indiceToIntensity.int2FloatEntrySet().iterator();
              Int2FloatMap.Entry nextInd2IntensityEntry;
              while (indiceToIntensityIt.hasNext()){
                nextInd2IntensityEntry = indiceToIntensityIt.next();
                int nextIndice = nextInd2IntensityEntry.getIntKey();
                massIntensityMap.put(indiceToMassMap.get(nextIndice), nextInd2IntensityEntry.getFloatValue());
              }
//              long step24 = System.currentTimeMillis();
//              time_indiceToMassMapS3 += step24-step23;//--> VDS-TIME: Ecoli (10Go) ~6-7mins
              scanMsMsDataMap.put(scanId,massIntensityMap);
          }
//          long end = System.currentTimeMillis();
          //--> VDS-TIME:  Ecoli (10Go). FROM step2 In "Timstof2MzDB" ~15.2min // = time_indiceToMassMapS2 +  time_indiceToMassMapS3 + scanMsMsDataMap.put: ~ 6-7min
//          time_indiceToMassMap += end- step22;
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
//                    System.out.println(" Scans "+scanAsDbl[i]+" => Ion Mobility "+ionMobilities[i]);
//                }
//            }

          //-> VDS For timing logs
//          if(nbrRead % 100 == 0){
//              LOG.debug(" TIME reading "+nbrRead+" frame(s):\tScans data read\t"+time_readScans+ "\tExtract peaks from buffer \t"+time_extractPeaks+"\tGet masses from indexes\t"+time_indiceToMass+"\tmap from indexes to mass\t"+time_indiceToMassMapS1+" - "+time_indiceToMassMapS2+" - "+time_indiceToMassMapS3+" - "+time_indiceToMassMap);
//              time_readScans = 0;
//              time_extractPeaks = 0;
//              time_indiceToMass=0;
//              time_indiceToMassMap=0;
//              time_indiceToMassMapS1=0;
//              time_indiceToMassMapS2=0;
//              time_indiceToMassMapS3=0;
//          }
        }   //End for all Frames
    }

    public TDFLibrary getTDFLib() {
        return m_tdfLib;
    }

    public Int2ObjectMap<Precursor> getPrecursorInfoById(long fileHandle){
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);

        return metaDataReader.getPrecursorInfoById();
    }

    public Map<String, String> readGlobalProperties(long fileHandle){
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);
        return metaDataReader.readGlobalMetaData();
    }

    public List<Pair<Integer, Double>> getIonMobilityIndexes(long fileHandle) {
        if(!m_cachedMetaReaderByHandle.containsKey(fileHandle))
            m_cachedMetaReaderByHandle.put(fileHandle, new TDFMetadataReader(m_ttFilesByHandle.get(fileHandle)));
        TDFMetadataReader metaDataReader =m_cachedMetaReaderByHandle.get(fileHandle);
        final Map<String, String> propertyGroup = metaDataReader.readPropertyGroup(1);
        int n = Integer.parseInt(propertyGroup.get("IMS_Cycle_RampTime_Trig")) + 1;
        double[] scanAsDbl = new double [n];
        double[] ionMobilities = new double [n];
        for(int i = 0; i < n; i++){
            scanAsDbl[i] = i;
        }
        long error_stat = m_tdfLib.tims_scannum_to_oneoverk0(fileHandle, 1, scanAsDbl , ionMobilities, ionMobilities.length);
        if (0 == error_stat) {
            LOG.error(" !!! could not convert scans to ion mobility for frame 1");
        } else {
            List<Pair<Integer, Double>> indices = new ArrayList<>(n);
            for(int i = 0; i < n; i++){
                indices.add(new ImmutablePair<>((int)scanAsDbl[i], ionMobilities[i]));
            }
            return indices;
        }
        return new ArrayList();
    }

 }
