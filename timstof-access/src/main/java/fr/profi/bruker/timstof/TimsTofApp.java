package fr.profi.bruker.timstof;

import fr.profi.bruker.timstof.io.TimstofReader;
import fr.profi.bruker.timstof.model.*;
import org.apache.commons.collections4.ListUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

public class TimsTofApp {

//    private static final String DEFAULT_SAMPLE_DIR = "target\\test-classes\\samples_data.d";
//    static String MY_SAMPLE_DIR =  "C:\\vero\\DEV\\TimsTof\\example_data\\Ecoli10ng_2715\\Ecoli10ng60minLCGrad_nanoElute_Slot1-46_01_2715.d";
    static String MY_SAMPLE_DIR =  "C:\\vero\\DEV\\TimsTof\\example_data\\200ngHela_compressed\\200ngHeLaPASEF_2min_compressed.d";
    private static Logger LOG = LoggerFactory.getLogger(TimsTofApp.class);

    public static void readSeqFrame(Long fileHdl, List<AbstractTimsFrame> frames){
        TimstofReader reader = TimstofReader.getTimstofReader();

        List<TimsPASEFFrame> pasefFrames = new ArrayList<>();
        for(AbstractTimsFrame fr : frames){
            AbstractTimsFrame.ScanMode scanMode = fr.getScanMode();
            LOG.info(" Reading "+scanMode+" Frame "+fr.getId()+": peaks = "+fr.getNbrPeaks() +" at "+fr.getTime());

            AbstractTimsFrame.MsMsType msMsType = fr.getMsmsType();
            switch (msMsType){
                case MS:
                    LOG.info(" MS : BPC "+fr.getMaxIntensity()+" / TIC "+fr.getSummedIntensity());
                    if(pasefFrames.size()>0) { //read Pasef file since last MS Frame
                        reader.fillFramesWithMsMsInfo(fileHdl, pasefFrames);
                        LOG.info(" Read "+pasefFrames.size()+" since last MS Frame : ");
                        for(TimsPASEFFrame pasefFr : pasefFrames){
                            for(PasefMsMsData msmsInfo : pasefFr.getPasefMsMSData()){
                                LOG.info(" Pasef Frame "+msmsInfo.getFrameId()+" Precursor "+msmsInfo.getPrecursorId()+" Start Scan "+msmsInfo.getStartScan()+" EndScan "+msmsInfo.getEndScan());
                            }
                        }
                        pasefFrames.clear();
                    }
                    break;
                case PASEF:
                    LOG.info(" PASEF : BPC "+fr.getMaxIntensity()+" / TIC "+fr.getSummedIntensity());
                    pasefFrames.add((TimsPASEFFrame)fr);
                    break;
                default:
                    LOG.info(" NOT Supported MSMS Type "+msMsType+": BPC "+fr.getMaxIntensity()+" / TIC "+fr.getSummedIntensity());
                    break;
            }
        }
    }

    public static void readMSFrameInfo(Long fileHdl, List<AbstractTimsFrame> frames){

        List<AbstractTimsFrame> msFrame = frames.stream().filter(f -> f.getMsmsType().equals(AbstractTimsFrame.MsMsType.MS)).collect(Collectors.toList());
        for(AbstractTimsFrame fr : msFrame) {
            LOG.info("\n Frame "+fr.getId()+"\tBPC: "+fr.getMaxIntensity()+"\tTIC: "+fr.getSummedIntensity()+" Time "+fr.getTime()/60);
        }
    }

    public static void readMSMSDataFromPrecursors(Long fileHdl, int ms1Index){
        //For MS Frame 1...
        TimstofReader reader =TimstofReader.getTimstofReader();

        List<Precursor> frame1Precs = reader.getPrecursorInfoById(fileHdl).values().stream().filter(p -> p.getParentMsFrame() == ms1Index).collect(Collectors.toList());
        long[] precIds = new long[frame1Precs.size()];
        for(int i =0 ;i <frame1Precs.size();i++)
            precIds[i] = frame1Precs.get(i).getId();

                //Read MSMS for these precursor
        MsMsCallbacks.MultipleMsMsData msmsData = new MsMsCallbacks.MultipleMsMsData();
        long error_stat = reader.getTDFLib().tims_read_pasef_msms(fileHdl,precIds,precIds.length,msmsData);
        if (0 == error_stat) {
            LOG.error(" !!! could not get msms spectrum for precursor ids {}", precIds);
        }

        Map<Long, MsMsCallbacks.MsMsSpectrum> msmsSpectra = msmsData.getMsMsSpectra();
        for(Map.Entry<Long, MsMsCallbacks.MsMsSpectrum>  e : msmsSpectra.entrySet()) {
            System.out.printf("msms data, precursor_id %d, number of peaks %d", e.getKey(), e.getValue().nbPeaks).println();
            System.out.println(Arrays.toString(e.getValue().mz_values));
            System.out.println(Arrays.toString(e.getValue().intensity_values));
//                double[] scanMasses = e.getValue().mz_values;
//                float[] scanIntensities = e.getValue().intensity_values;
//                for(int j =0; j<scanMasses.length ; j++)
//                    System.out.println(scanMasses[j]+"\t"+scanIntensities[j]);
        }
    }

    private  static void cleanFrameData(List<AbstractTimsFrame> frames){
        frames.forEach(AbstractTimsFrame::clearSpectraData);
    }

    public static void readFramesDataAndClean(Long fileHdl, List<AbstractTimsFrame> allFrames){
        long start = System.currentTimeMillis();
        //For Frame associated to MS Frame 2...
        long step2;
        long stepClean = 0L;
        TimstofReader reader = TimstofReader.getTimstofReader();
        List<TimsPASEFFrame> pasefFrames = new ArrayList<>();
        allFrames.forEach(fr -> {
            if(fr.getMsmsType().equals(AbstractTimsFrame.MsMsType.PASEF))
                pasefFrames.add( (TimsPASEFFrame) fr);
        });

        reader.fillFramesWithMsMsInfo(fileHdl, pasefFrames);
        long step1 = System.currentTimeMillis();
        LOG.info("End All frames fillFramesWithMsMsInfo: " +(step1-start));
        //Read Frames data by 2000
        List<List<AbstractTimsFrame>> pMsMsFrame = ListUtils.partition(allFrames,2000);
        for (List<AbstractTimsFrame> abstractTimsFrames : pMsMsFrame) {
            reader.fillFramesWithSpectrumData(fileHdl, abstractTimsFrames);
            long step1a = System.currentTimeMillis();
            TimsTofApp.cleanFrameData(abstractTimsFrames);
            long step1b = System.currentTimeMillis();
            stepClean += step1b - step1a;
        }

        step2 = System.currentTimeMillis();
        LOG.info("End All frames fillFramesWithSpectrumData: " +(step2-step1));
        LOG.info(" in which clean Frames Data: " +stepClean);
    }

    public static void readMSMSDataFromFrames(Long fileHdl, List<TimsPASEFFrame> msmsFrames){
        long start = System.currentTimeMillis();
        //For Frame associated to MS Frame 2...
        long step2;

        TimstofReader reader = TimstofReader.getTimstofReader();
        reader.fillFramesWithMsMsInfo(fileHdl, msmsFrames);
        long step1 = System.currentTimeMillis();
        LOG.info("End MS2  fillFramesWithMsMsInfo: " +(step1-start));
        //Read MSMS for these ms2Frames
        reader.fillFramesWithSpectrumData(fileHdl, msmsFrames);
        step2 = System.currentTimeMillis();
        LOG.info("End MS2  fillFramesWithSpectrumData: " +(step2-step1));

        for( TimsPASEFFrame fr : msmsFrames) {
            List<PasefMsMsData>  data = fr.getPasefMsMSData();
            LOG.debug("ms/ms data, frame {}, msmsType {}, number of peaks {}, number PASEF Fr {}", fr.getId(), fr.getMsmsType().getMsMsTypeCode(), fr.getNbrPeaks(), data.size());

//            List<Double> masses = new ArrayList<>();
//            List<Float> intensities = new ArrayList<>();
//            boolean b = fr.isPasef();
//            System.out.printf(" Frame {}, {} PasefMsMS : ",fr.getId(), data.size());
            data.forEach(pasefInfo -> {
                double isolMz = pasefInfo.getIsolationMz();
                Spectrum sp = pasefInfo.getPasefSpectrum();
                if(sp !=null) {
                    //                    fillArrayFromSpectra(sp, masses, intensities);
                    double[] pasefmasses = sp.getMasses();
                    LOG.debug("\tINFOO\tPasefMsMS\t{}\tmz\t{}\tstart\t{}\tend\t{}\tmasses/intensities size\t{}",fr.getId(),isolMz, pasefInfo.getStartScan(), pasefInfo.getEndScan(), pasefmasses.length);
                    LOG.debug(Arrays.toString(pasefmasses));
                    LOG.debug(Arrays.toString(sp.getIntensities()));
                } else
                    LOG.debug(" PasefMsMS mz {}, start {}, end {}, NO masses/intensities.",isolMz, pasefInfo.getStartScan(), pasefInfo.getEndScan());
            });
//            LOG.info(" Frame {}, all peaks : ",fr.getId());
//            System.out.println(masses);
//            System.out.println(intensities);
        }
        long end = System.currentTimeMillis();
        LOG.info("End gal MS2  readMSMSDataFromFrames " +(end-step2));
    }

    public static void readMSDataFromFrames(Long fileHdl, List<TimsMSFrame> msFrames){
        long start = System.currentTimeMillis();

        long step2;

        //For Frame associated to MS Frame 1...
        TimstofReader reader = TimstofReader.getTimstofReader();
//        reader.fillFramesWithMsMsInfo(fileHdl, msFrames);
//        long step1 = System.currentTimeMillis();
//        LOG.info("End MS1  fillFramesWithMsMsInfo: " +(step1-start));

        //Read Spectrum Data for these msFrames
        reader.fillFramesWithSpectrumData(fileHdl, msFrames);
        step2 = System.currentTimeMillis();
        LOG.info("End MS1  fillFramesWithSpectrumData: " +(step2-start));

//        for( TimsMSFrame fr : msFrames) {
//
//            System.out.printf("MS data, frame %d, number of peaks %d", fr.getId(), fr.getNbrPeaks()).println();
//            List<Spectrum>  data = fr.getAllSpectra();
//            List<Double> masses = new ArrayList<>();
//            List<Float> intensities = new ArrayList<>();
//            LOG.info(" Frame {}, {} Spectrum : ",fr.getId(), data.size());
//            data.forEach(sp-> {
//                fillArrayFromSpectra(sp, masses, intensities);
//            });
//            LOG.info(" Frame {}, all peaks : ",fr.getId());
//            System.out.println(masses);
//            System.out.println(intensities);
//        }
        long end = System.currentTimeMillis();
        LOG.info("End gal MS1  readMSDataFromFrames " +(end-step2));
    }

    private static void fillArrayFromSpectra(Spectrum sp,List<Double> masses, List<Float> intensities ){
        double[] spMasses = sp.getMasses();
        float[] spInten = sp.getIntensities();
//        System.out.println(Arrays.toString(spMasses));
//        System.out.println(Arrays.toString(spInten));
        System.out.println(" peaks # "+spInten.length);
        for(int i =0; i<spMasses.length; i++){
            masses.add(spMasses[i]);
            intensities.add(spInten[i]);
        }
    }
//
//    public static void readFullFrameInfo(Long fileHdl, List<TimsFrame> frames ){
//
//        if(m_useJNR) {
//            TimstofReaderJNR readerJNR = TimstofReaderJNR.getTimstofReader();
//            LOG.info(" START Read FrameMsMsInfo ");
//            readerJNR.fillFramesWithMsMsInfo(fileHdl,frames);
//            LOG.info(" END Read FrameMsMsInfo ");
//        } else {
//            TimstofReader reader = TimstofReader.getTimstofReader();
//            LOG.info(" START Read FrameMsMsInfo ");
//            reader.fillFramesWithMsMsInfo(fileHdl,frames);
//            LOG.info(" END Read FrameMsMsInfo ");
//        }
//    }

    public static void readPrecursorInfo(Long fileHdl){

        TimstofReader reader =  TimstofReader.getTimstofReader();

        Collection<Precursor> allPrec = reader.getPrecursorInfoById(fileHdl).values();
        Map<Integer, List<Precursor>> precByFrame = allPrec.stream().collect(Collectors.groupingBy(Precursor::getParentMsFrame));
        List<Integer> parentMsFr = new ArrayList<>(precByFrame.keySet());
        parentMsFr.sort(Comparator.naturalOrder());
        for(Integer parentFrId : parentMsFr){
            StringBuilder sb  = new StringBuilder("\n In MS Frame ").append(parentFrId).append(": ");
            List<Precursor> framePrecs = precByFrame.get(parentFrId);
            for(Precursor p: framePrecs){
                sb.append("\n - Precursor ").append(p.getId());
                sb.append(":\tcharge= ").append(p.getCharge());
                sb.append("\tintensity= ").append(p.getIntensity());
                sb.append("\tmobility(scan number)= ").append(p.getScanNumber());
            }
            LOG.info(sb.toString());
        }
    }

        public static void  main(String[] argv){

        //TimsTofApp.m_useJNR = true;
        String filePath = MY_SAMPLE_DIR;//DEFAULT_SAMPLE_DIR;
        String cmd ="msmsFrame";
        if(argv != null && argv.length>=2) {
            filePath = argv[0];
            cmd = argv[1];
        } else if(argv != null && argv.length==1) {
            filePath = argv[0];
        }

        File f = new File(filePath);
        LOG.info("File  "+f.getAbsolutePath()+" exist "+f.exists());

        Long fileHdl = TimstofReader.getTimstofReader().openTimstofFile(f);

        List<AbstractTimsFrame> frames ;
        switch (cmd) {
            case "seq": //read Frame sequentially and print info
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
                TimsTofApp.readSeqFrame(fileHdl,frames);
                break;

            case "ms": // Read only MS Frame info
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
                TimsTofApp.readMSFrameInfo(fileHdl,frames);
                break;

            case "precursor": //read Precursor info
                TimsTofApp.readPrecursorInfo(fileHdl);
                break;

            case "msmsPrecursor": //Read MSMS referencing specific precursor... hard coded
                TimsTofApp.readMSMSDataFromPrecursors(fileHdl,1);
                break;

            case "allFrames":
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
                TimsTofApp.readFramesDataAndClean(fileHdl,frames);
                break;

            case "msmsFrame": //Read ALL MSMS // between 2 first Ms or a
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
//                List<TimsFrame> ms2Frames = frames.stream().filter(fr -> fr.getId() ==812).collect(Collectors.toList());
                List<TimsPASEFFrame> ms2Frames = new ArrayList<>();
                 frames.forEach(fr -> {
                    if(fr.getMsmsType().equals(AbstractTimsFrame.MsMsType.PASEF) ) //&& fr.getId() >138 && fr.getId()<150)
                        ms2Frames.add((TimsPASEFFrame) fr);
                });
                TimsTofApp.readMSMSDataFromFrames(fileHdl,ms2Frames);
                break;

            case "msFrame": //Read third MS Frame ... hard coded
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
                List<TimsMSFrame> msFr = new ArrayList<>();
                frames.forEach(fr -> {
                    if(fr.getMsmsType().equals(AbstractTimsFrame.MsMsType.MS))
                        msFr.add((TimsMSFrame)fr);
                });
//                List<TimsFrame> l = new ArrayList();
//                l.add(msFr.get(0));
                TimsTofApp.readMSDataFromFrames(fileHdl,msFr);
                break;

            case "fullFrame":
                TimstofReader.getTimstofReader().getFullTimsFrames(fileHdl);
                LOG.info(" END Read Full Frame Info ");
                break;

            case "singleSpectrum":
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                List<TimsMSFrame> msFrames = new ArrayList<>();
                frames.forEach(fr -> {
                    if(fr.getMsmsType().equals(AbstractTimsFrame.MsMsType.MS))
                        msFrames.add((TimsMSFrame)fr);
                });
//                List<TimsFrame> l = new ArrayList();
//                TimsFrame fr = msFr.get(1);
//                l.add(fr);
                TimsTofApp.readMSDataFromFrames(fileHdl,msFrames);
                for(TimsMSFrame tf : msFrames) {
                    tf.getSingleSpectrum(SpectrumGeneratingMethod.SMOOTH);
                }
        }
    }



}
