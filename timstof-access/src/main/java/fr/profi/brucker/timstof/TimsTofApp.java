package fr.profi.brucker.timstof;

import fr.profi.brucker.timstof.io.TimstofReader;
import fr.profi.brucker.timstof.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

public class TimsTofApp {

    private static final String DEFAULT_SAMPLE_DIR = "target\\test-classes\\samples_data.d";
    static String MY_SAMPLE_DIR =  "C:\\vero\\DEV\\TimsTof\\example_data\\100ngHela_snippet\\HeLa_default_100ng_Slot1-7_01_531_snippet.d";
    private static Logger LOG = LoggerFactory.getLogger(TimsTofApp.class);

    public static void readSeqFrame(Long fileHdl, List<TimsFrame> frames){
        TimstofReader reader = TimstofReader.getTimstofReader();

        List<TimsFrame> pasefFrames = new ArrayList<>();
        for(TimsFrame fr : frames){
            TimsFrame.ScanMode scanMode = fr.getScanMode();
            LOG.info(" Reading "+scanMode+" Frame "+fr.getId()+": peaks = "+fr.getNbrPeaks() +" at "+fr.getTime());

            TimsFrame.MsMsType msMsType = fr.getMsmsType();
            switch (msMsType){
                case MS:
                    LOG.info(" MS : BPC "+fr.getMaxIntensity()+" / TIC "+fr.getSummedIntensity());
                    if(pasefFrames.size()>0) { //read Pasef file since last MS Frame
                        reader.fillFramesWithMsMsInfo(fileHdl, pasefFrames);
                        LOG.info(" Read "+pasefFrames.size()+" since last MS Frame : ");
                        for(TimsFrame pasefFr : pasefFrames){
                            for(PasefMsMsData msmsInfo : pasefFr.getPasefMsMSData()){
                                LOG.info(" Pasef Frame "+msmsInfo.getFrameId()+" Precursor "+msmsInfo.getPrecursorId()+" Start Scan "+msmsInfo.getStartScan()+" EndScan "+msmsInfo.getEndScan());
                            }
                        }
                        pasefFrames.clear();
                    }
                    break;
                case PASEF:
                    LOG.info(" PASEF : BPC "+fr.getMaxIntensity()+" / TIC "+fr.getSummedIntensity());
                    pasefFrames.add(fr);
                    break;
                default:
                    LOG.info(" NOT Supported MSMS Type "+msMsType+": BPC "+fr.getMaxIntensity()+" / TIC "+fr.getSummedIntensity());
                    break;
            }
        }
    }

    public static void readMSFrameInfo(Long fileHdl, List<TimsFrame> frames){

        List<TimsFrame> msFrame = frames.stream().filter(f -> f.getMsmsType().equals(TimsFrame.MsMsType.MS)).collect(Collectors.toList());
        for(TimsFrame fr : msFrame) {
            LOG.info("\n Frame "+fr.getId()+"\tBPC: "+fr.getMaxIntensity()+"\tTIC: "+fr.getSummedIntensity()+" Time "+fr.getTime()/60);
        }
    }

    public static void readMSMSDataFromPrecursors(Long fileHdl, int ms1Index){
        //For MS Frame 1...
        TimstofReader reader =TimstofReader.getTimstofReader();

        List<Precursor> frame1Precs = reader.readPrecursorInfo(fileHdl).stream().filter(p -> p.getParentMsFrame().equals(ms1Index)).collect(Collectors.toList());
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

    public static void readMSMSDataFromFrames(Long fileHdl, List<TimsFrame> msFrames){
        long start = System.currentTimeMillis();
        //For Frame associated to MS Frame 2...
        long step2 =0l;

        TimstofReader reader = TimstofReader.getTimstofReader();
        reader.fillFramesWithMsMsInfo(fileHdl, msFrames);
        long step1 = System.currentTimeMillis();
        LOG.info("End MS2  fillFramesWithMsMsInfo: " +(step1-start));
        //Read MSMS for these ms2Frames
        reader.fillFramesWithSpectrumData(fileHdl, msFrames);
        step2 = System.currentTimeMillis();
        LOG.info("End MS2  fillFramesWithSpectrumData: " +(step2-step1));

//        for( TimsFrame fr : msFrames) {
//
//            System.out.printf("ms/ms data, frame %d, msmsType %d, number of peaks %d", fr.getId(), fr.getMsmsType().getMsMsTypeCode(), fr.getNbrPeaks()).println();
//            List<PasefMsMsData>  data = fr.getPasefMsMSData();
//            List<Double> masses = new ArrayList<>();
//            List<Float> intensities = new ArrayList<>();
//            boolean b = fr.isPasef();
////            LOG.info(" Frame {}, {} PasefMsMS : ",fr.getId(), data.size());
////            data.forEach(pasefInfo -> {
////                double isolMz = pasefInfo.getIsolationMz();
////                Spectrum sp = pasefInfo.getPasefSpectrum();
////                LOG.info(" PasefMsMS mz {}, start {}, end {}, masses/intensities: ",pasefInfo.getIsolationWidth(), pasefInfo.getStartScan(), pasefInfo.getEndScan());
////                if(sp !=null)
////                    fillArrayFromSpectra(sp, masses, intensities);
////            });
////            LOG.info(" Frame {}, all peaks : ",fr.getId());
////            System.out.println(masses);
////            System.out.println(intensities);
//        }
        long end = System.currentTimeMillis();
        LOG.info("End gal MS2  readMSMSDataFromFrames " +(end-step2));
    }

    public static void readMSDataFromFrames(Long fileHdl, List<TimsFrame> msFrames){
        long start = System.currentTimeMillis();

        long step2 =0l;

        //For Frame associated to MS Frame 1...
        TimstofReader reader = TimstofReader.getTimstofReader();
        reader.fillFramesWithMsMsInfo(fileHdl, msFrames);
        long step1 = System.currentTimeMillis();
        LOG.info("End MS1  fillFramesWithMsMsInfo: " +(step1-start));
        //Read MSMS for these ms2Frames
        reader.fillFramesWithSpectrumData(fileHdl, msFrames);
        step2 = System.currentTimeMillis();
        LOG.info("End MS1  fillFramesWithSpectrumData: " +(step2-step1));

//        for( TimsFrame fr : msFrames) {
//
//            System.out.printf("ms data, frame %d, msmsType %d, number of peaks %d", fr.getId(), fr.getMsmsType().getMsMsTypeCode(), fr.getNbrPeaks()).println();
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

        List<Precursor> allPrec = reader.readPrecursorInfo(fileHdl);
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

    //C:\vero\DEV\TimsTof\example_data\Ecoli10ng60minLCGrad_nanoElute_Slot1-46_01_2715.d //
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

        List<TimsFrame> frames ;
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
            case "msmsFrame": //Read MSMS between 2 first Ms
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
//                List<TimsFrame> ms2Frames = frames.stream().filter(fr -> fr.getId() ==812).collect(Collectors.toList());
                List<TimsFrame> ms2Frames = frames.stream().filter(fr -> fr.getMsmsType().equals(TimsFrame.MsMsType.PASEF)).collect(Collectors.toList());
                TimsTofApp.readMSMSDataFromFrames(fileHdl,ms2Frames);
                break;
            case "msFrame": //Read third MS Frame ... hard coded
                frames  = TimstofReader.getTimstofReader().getTimsFrames(fileHdl);
                LOG.info(" END Read Frame Info ");
                List<TimsFrame> msFr = frames.stream().filter(fr -> fr.getMsmsType().equals(TimsFrame.MsMsType.MS)).collect(Collectors.toList());
//                List<TimsFrame> l = new ArrayList();
//                l.add(msFr.get(0));
                TimsTofApp.readMSDataFromFrames(fileHdl,msFr);
                break;
            case "fullFrame":
                frames  = TimstofReader.getTimstofReader().getFullTimsFrames(fileHdl);
                LOG.info(" END Read Full Frame Info ");
                break;

        }
    }

}
