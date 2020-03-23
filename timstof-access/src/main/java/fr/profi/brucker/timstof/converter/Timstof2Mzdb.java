package fr.profi.brucker.timstof.converter;

import com.github.mzdb4s.db.model.*;
import com.github.mzdb4s.db.model.params.*;
import com.github.mzdb4s.db.model.params.param.CVParam;
import com.github.mzdb4s.db.model.params.param.PsiMsCV;
import com.github.mzdb4s.db.model.params.param.UserParam;
import com.github.mzdb4s.io.writer.MzDbWriter;
import com.github.mzdb4s.msdata.*;
import com.github.sqlite4s.SQLiteFactory$;
import fr.profi.brucker.timstof.io.TimstofReader;
import fr.profi.brucker.timstof.model.TimsFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.Some;
import scala.collection.JavaConverters;

import java.io.File;
import java.nio.ByteOrder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class Timstof2Mzdb {

    private final static Logger LOG = LoggerFactory.getLogger(Timstof2Mzdb.class);
    static String filepath =  "c:\\vero\\DEV\\TimsTof\\example_data\\Ecoli10ng60minLCGrad_nanoElute_Slot1-46_01_2715.d";
//    static String filepath =  "c:\\vero\\DEV\\TimsTof\\example_data\\200ngHeLaPASEF_2min_compressed.d";

    static {
        try {
            System.load("C:\\vero\\DEV\\sqlite4java-win32-x64-1.0.392.dll");
        } catch (UnsatisfiedLinkError e) {
            System.err.println("Native code library failed to load.\n" + e);
            System.exit(1);
        }
    }

    private File m_ttFile;
    private long m_fileHdl;
    private TimstofReader m_ttReader;

    //For all Spectra Index, map to associated Frame Index
    private Map<Integer, Integer> m_spectra2FrameIndex;
    //Memorize first Spectra Index for each Frame (index)
    private Map<Integer, Integer> m_frame2FirstSpectraIndex;
    private Map<Integer, Integer> m_frame2ReadSpectraCount;

    private List<TimsFrame>  m_frames;
    private List<fr.profi.brucker.timstof.model.Precursor> m_precursors;
    DataEncoding m_profileDataEnconding;
    DataEncoding m_centroidDataEnconding;
    DataEncoding m_fittedDataEnconding;

    //VD TODO: open reader in constructor ...
    public Timstof2Mzdb(File ttFile, Long fileHdl, TimstofReader reader){
        this.m_ttFile = ttFile;
        this.m_fileHdl = fileHdl;
        this.m_ttReader = reader;
        initFramesData();
        m_profileDataEnconding = new DataEncoding(-1, DataMode.PROFILE(), PeakEncoding.HIGH_RES_PEAK(), "none", ByteOrder.LITTLE_ENDIAN);
        m_centroidDataEnconding = new DataEncoding(-1, DataMode.CENTROID(), PeakEncoding.HIGH_RES_PEAK(), "none", ByteOrder.LITTLE_ENDIAN);
        m_fittedDataEnconding = new DataEncoding(-1, DataMode.FITTED(), PeakEncoding.HIGH_RES_PEAK(), "none", ByteOrder.LITTLE_ENDIAN);
    }


    private void initFramesData(){
        long start = System.currentTimeMillis();
        //Read TimsFrames with associated MetaData
        m_frames = m_ttReader.getFullTimsFrames(m_fileHdl);
        Collections.sort(m_frames);

        //Init indexes map
        Integer spectrumIndex =1;
        m_spectra2FrameIndex = new HashMap<>();
        m_frame2FirstSpectraIndex = new HashMap<>();
        for(TimsFrame tf : m_frames){
            int nbrSpectrum = (!tf.isPasef()) ? 1 : tf.getPasefMsMSData().size(); //VDS TODO For now 1 spectrum for MS ==> May have one per scans groups!!

            m_frame2FirstSpectraIndex.put(tf.getId(),spectrumIndex);
            for(int i=0;i<nbrSpectrum; i++){
                m_spectra2FrameIndex.put(spectrumIndex, tf.getId());
                spectrumIndex++;
            }
        }
        m_frame2ReadSpectraCount = new HashMap<>();
        m_precursors = m_ttReader.readPrecursorInfo(m_fileHdl);
        long end  = System.currentTimeMillis();
        LOG.info("Read meta data for "+m_frames.size()+ " frames and "+m_spectra2FrameIndex.size()+" spectrum. Duration : "+ (end-start) +" ms");
    }

    private MzDbMetaData createMzDbMetaData(){

        ParamTree pt = new ParamTree();
        List<UserParam> ups = new ArrayList<>();
        ups.add(new UserParam("origin_file_format", "Timstof Brucker","xsd:string"));
        pt.setUserParams(JavaConverters.asScalaIteratorConverter(ups.iterator()).asScala().toSeq());
        int currentTime = new Long(System.currentTimeMillis()).intValue();
        MzDbHeader mzdbHeader = new MzDbHeader("0.7", currentTime,  pt);

        List<DataEncoding> des = new ArrayList<>();
        des.add(m_profileDataEnconding);
        des.add(m_centroidDataEnconding);
        des.add(m_fittedDataEnconding);

        //Read GlocalPropertie
        Map<String, String> globalProperties = m_ttReader.readGlobalProperties(m_fileHdl);

        //CommonInstrumentParams ex
        /* <referenceableParamGroup id="CommonInstrumentParams">
      <cvParam cvRef="MS" accession="MS:1001911" name="Q Exactive" value=""/>
      <cvParam cvRef="MS" accession="MS:1000529" name="instrument serial number" value="Exactive Series slot #2533"/>
    </referenceableParamGroup> */
        scala.Option noneOp = scala.None$.MODULE$;

        CommonInstrumentParams ciParams  = new CommonInstrumentParams(-1,new ParamTree());
        List<Component> compos = new ArrayList<>();
        SourceComponent srcCompo = new SourceComponent(1);
        //Hard CODED for exemple !
        List<CVParam> params = new ArrayList<>();
        //<cvParam cvRef="MS" accession="MS:1000398" name="nanoelectrospray" value=""/>
        params.add(new CVParam("MS:1000398", "nanoelectrospray", "", "MS", noneOp, noneOp, noneOp));
        //<cvParam cvRef="MS" accession="MS:1000485" name="nanospray inlet" value=""/>
        params.add(new CVParam("MS:1000485", "nanospray inlet", "", "MS", noneOp, noneOp, noneOp));
        srcCompo.setCVParams(JavaConverters.asScalaIteratorConverter(params.iterator()).asScala().toSeq());
        compos.add(srcCompo);
        //VDS TODO  Add Analyzer and Detector component
        ComponentList compList = new ComponentList( JavaConverters.asScalaIteratorConverter(compos.iterator()).asScala().toSeq());

        InstrumentConfiguration iconf = new InstrumentConfiguration(-1,"test",1 , new ParamTree(),compList);
        List<InstrumentConfiguration> iConfList = new ArrayList<>();
        iConfList.add(iconf);

        int indexSoft = 1;
        List<Software> softList = new ArrayList<>();
        ParamTree processMethodParamTree = new ParamTree();
        List<ProcessingMethod> pmList = new ArrayList<>();
        if(globalProperties.containsKey("AcquisitionSoftware")){
            String name = globalProperties.get("AcquisitionSoftware");
            String version = globalProperties.getOrDefault("AcquisitionSoftwareVersion", "Unknown");
            Software soft = new Software(indexSoft++,name,version, new ParamTree());
            softList.add(soft);
        }
        ProcessingMethod pm = new ProcessingMethod(1,0,"TimsTof_2_mzdb_conversion" ,scala.Option.apply(processMethodParamTree), indexSoft);
        pmList.add(pm);
        softList.add( new Software(indexSoft++,"ttofConverter","0.1", new ParamTree()));

        pm = new ProcessingMethod(2,1,"TimsTof_2_mzdb_conversion" ,scala.Option.apply(processMethodParamTree), indexSoft);
        pmList.add(pm);
        softList.add( new Software(indexSoft,"mzDB","0.9.10", new ParamTree()));

        String acqDateStr = globalProperties.get("AcquisitionDateTime");
        Date acqDate =  Calendar.getInstance().getTime();
        if(acqDateStr!=null) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
            try {
                acqDate = sdf.parse(acqDateStr);
            } catch (ParseException e) {
                acqDate = Calendar.getInstance().getTime();
            }
        }

        String fileName =m_ttFile.getName();
        int pos = fileName.lastIndexOf(".");
        if (pos > 0 && pos < (fileName.length() - 1)) { // If '.' is not the first or last character.
            fileName = fileName.substring(0, pos);
        }

        Run r= new Run(-1,fileName, acqDate);
        List<Run> rList = new ArrayList<>();
        rList.add(r);

        String splName = globalProperties.getOrDefault("SampleName", m_ttFile.getName()+"_Sample");
        Sample s = new Sample(-1,splName, new ParamTree());
        List<Sample> sList = new ArrayList<>();
        sList.add(s);

        SourceFile sFile  = new SourceFile(1,m_ttFile.getName(),m_ttFile.getAbsolutePath(), new ParamTree());
        List<SourceFile> sFileList = new ArrayList<>();
        sFileList.add(sFile);

        LOG.info("Created MzDbMetaData.");
        return new MzDbMetaData(mzdbHeader, JavaConverters.asScalaIteratorConverter(des.iterator()).asScala().toSeq(), ciParams, JavaConverters.asScalaIteratorConverter(iConfList.iterator()).asScala().toSeq(), JavaConverters.asScalaIteratorConverter(pmList.iterator()).asScala().toSeq(), JavaConverters.asScalaIteratorConverter(rList.iterator()).asScala().toSeq(), JavaConverters.asScalaIteratorConverter(sList.iterator()).asScala().toSeq(), JavaConverters.asScalaIteratorConverter(softList.iterator()).asScala().toSeq(), JavaConverters.asScalaIteratorConverter(sFileList.iterator()).asScala().toSeq());
    }

    private void createMZdBData(){
        MzDbWriter writer = null;
        scala.Option noneOp = scala.None$.MODULE$;
        try {

            String outFilePath = m_ttFile.getAbsolutePath().toString()+"defBBSize.mzdb";
            File outFile = new File( outFilePath);
//            BBSizes newNBbSize = new BBSizes(10000, 10000,0,0);
            DefaultBBSizes$ bbsize = DefaultBBSizes$.MODULE$;
            SQLiteFactory$ sf = SQLiteFactory$.MODULE$;

            MzDbMetaData mzDbMetaData =  createMzDbMetaData();
            writer = new MzDbWriter(outFile, mzDbMetaData, /*newNBbSize*/  bbsize.apply(), false, sf);
            writer.open();

            int spId = 1; //Sprectrum Index start at 1
            int cycle =0; //VDS : TODO 1 cycle = 1Ms + xMSMS ?
            int nbrSp  = m_spectra2FrameIndex.size();

            //VDS: times to get duration debug info
            long time1 = 0;
            long time2 = 0;
            long time3 = 0;
            long time4 = 0;

            while(spId <= nbrSp) { //Assume spectrum id start at 1 and are incremental => done in initFrameData above
                long start = System.currentTimeMillis();  //VDS: times to get duration debug info

                Integer frameId = m_spectra2FrameIndex.get(spId);

                //increment nbr spectrum read for specific frameId
                int nbrScanForFrame  = m_frame2ReadSpectraCount.getOrDefault(frameId,0);
                nbrScanForFrame++;
                m_frame2ReadSpectraCount.put(frameId,nbrScanForFrame);

                //get TimsFrame with specific id
                Optional<TimsFrame> opFrame = m_frames.stream().filter(frame -> frame.getId().equals(frameId)).findFirst();
                if (!opFrame.isPresent()) {
                    LOG.warn("#### NO FRAME " + frameId+" for spectra "+spId);
                    spId++;
                    continue;
                }

                TimsFrame timsFrame = opFrame.get();
                if (!timsFrame.spectrumRead()) {
                    //get spectrum information if not read yet
                    List<TimsFrame> tfs = Collections.singletonList(timsFrame);
                    m_ttReader.fillFramesWithSpectrumData(m_fileHdl, tfs);
                }

                Precursor mzdbPrecursor = null;
                fr.profi.brucker.timstof.model.Spectrum ttSpectrum = null;
                Option preMz = noneOp;
                Option preCharge = noneOp;

                //VDS: times to get duration debug info
                long step1 = System.currentTimeMillis();
                time1 += step1-start;
                long step2 =0;

                int mslevel = 0;
                boolean errFound = false;
                switch (timsFrame.getMsmsType()) {
                    case MS:
                        // Ms Frame
                        mslevel = 1;
                        cycle++;
                        //Read 'single' spectrum ==> TODO change to read all Scans Spectrum or use groups ??
                        ttSpectrum = timsFrame.getSingleSpectrum();

                        //VDS: times to get duration debug info
                        step2 = System.currentTimeMillis();
                        time3 += step2 - step1;
                        break;

                    case PASEF:
                        mslevel = 2;
                        if( timsFrame.getPrecursorId() != null){
                            List<Integer> precursorIds = timsFrame.getPrecursorId();
                            Collections.sort(precursorIds);

                            //-- Read spectrum corresponding to index...
                            int indexInFrameSpectra = spId - m_frame2FirstSpectraIndex.get(timsFrame.getId()); //Index relative to frame
                            if (indexInFrameSpectra >= precursorIds.size() || indexInFrameSpectra <0) {
                                LOG.warn("#### WARNING #### can't found precursor id  for spectra " + spId+" Index in frame : "+indexInFrameSpectra+". TimsFrame index"+timsFrame.getId());
                                errFound = true;
                            } else {

                                //Get spectrum relative to index and specific to precursor
                                int precursorId = precursorIds.get(indexInFrameSpectra);
                                ttSpectrum = timsFrame.getPrecursorSpectrum(precursorId);
                                if (ttSpectrum == null) {
                                    LOG.warn("#### WARNING #### No precursor spectrum was defined for frame  " + timsFrame.getId() + "; precursorId : " + precursorId + ". Spectrum index " + spId);
                                    errFound = true;
                                } else {

                                    //Create mzDB Precursor using timstof Precursor as model
                                    Optional<fr.profi.brucker.timstof.model.Precursor> timstofPrecursor = m_precursors.stream().filter(pre -> pre.getId().equals(precursorId)).findFirst();
                                    mzdbPrecursor = new Precursor();
                                    mzdbPrecursor.spectrumRef_$eq(ttSpectrum.getTitle());

                                    if (timstofPrecursor.isPresent()) {
                                        fillmzDBPrecursor(mzdbPrecursor, timstofPrecursor.get(), String.valueOf(timsFrame.getPrecursorCollisionEnergy(precursorId)));
                                        preMz = Some.apply(timstofPrecursor.get().getMonoIsotopicMz());
                                        preCharge = Some.apply(timstofPrecursor.get().getCharge());
                                    }
                                }
                                //VDS: times to get duration debug info
                                step2 = System.currentTimeMillis();
                                time2 += step2 - step1;
                            }
                        } else {
                            LOG.warn("#### WARNING ####  UNSUPPORTED Frame type. Only MS1 and PASEF are supported actually. Frame "+timsFrame.getId()+" is getMsmsType "+timsFrame.getMsmsType());
                            errFound = true;
                        }
                        break;
                    case DIA:
                    case MSMS:
                    default:
                        LOG.warn("#### WARNING ####  UNSUPPORTED Frame type. Only MS1 and PASEF are supported actually. Frame "+timsFrame.getId()+" is getMsmsType "+timsFrame.getMsmsType());
                        errFound = true;
                        break;
                }

                if(errFound){
                    //All spectrum of current frame has been written. Can remove data for memory purpose
                    if(timsFrame.getPasefMsMSData() != null && timsFrame.getPasefMsMSData().size() == nbrScanForFrame){
                        timsFrame.clearSpectraData();
                    }
                    spId++;
                    continue;
                }

                float rt = timsFrame.getTime().floatValue();
                float[] intensities = ttSpectrum.getIntensities();
                double[] masses = ttSpectrum.getMasses();
                int nbPeaks = intensities.length;
                if (nbPeaks > 0) { //At least one peak ... VDS TODO or create an empty spectrum ?
                    int maxIndex = 0;
                    float prevIntensity = intensities[0];
                    for (int index = 1; index < nbPeaks; index++) {
                        float intAtIndex = intensities[index];
                        if (intAtIndex > prevIntensity) {
                            maxIndex = index;
                            prevIntensity = intAtIndex;
                        }
                    }

                    float spTIC = timsFrame.getSummedIntensity().floatValue();
//                    LOG.debug("write sp id " + spId + " of frame " + timsFrame.getId() + " Msl " + msl + " found in spectra max intensity " + intensities[maxIndex] + " in frame info " + timsFrame.getMaxIntensity());

                    SpectrumHeader spH = new SpectrumHeader(spId, spId, ttSpectrum.getTitle(), cycle, rt, mslevel, noneOp, nbPeaks, false, spTIC, ttSpectrum.getMasses()[maxIndex], intensities[maxIndex], preMz, preCharge, spId, (ScanList) null, mzdbPrecursor, noneOp);
                    com.github.mzdb4s.msdata.SpectrumData spData = new com.github.mzdb4s.msdata.SpectrumData(ttSpectrum.getMasses(), ttSpectrum.getIntensities());
                    com.github.mzdb4s.msdata.Spectrum mzdb4sSp = new com.github.mzdb4s.msdata.Spectrum(spH, spData);
                    SpectrumXmlMetaData spectrumMetaData = new SpectrumXmlMetaData(spId, "", "", scala.Option.empty(), scala.Option.empty());

                    DataEncoding spectrumDE = (timsFrame.getMsmsType().equals(TimsFrame.MsMsType.MS)) ? m_profileDataEnconding : m_centroidDataEnconding;
                    writer.insertSpectrum(mzdb4sSp, spectrumMetaData, spectrumDE);

                    //VDS: times to get duration debug info
                    long step4 = System.currentTimeMillis();
                    time4 += step4 - step2;
                }


                //All spectrum of current frame has been written. Can remove data for memory purpose
                if (timsFrame.getSpectrumCount() == nbrScanForFrame) {
                    timsFrame.clearSpectraData();
                }

                spId++;

                //VDS: times to get duration debug info
                if (spId % 10000 == 0 || spId == nbrSp) {
                    LOG.info("Already writed " + spId + " spectra on " + nbrSp);
                    LOG.info("Time used to read Frame: " + time1);
                    LOG.info("Time used to create MS2: " + time2);
                    LOG.info("Time used to create MS1: " + time3);
                    LOG.info("Time used to write data: " + time4);
                    time1 = 0;
                    time2 = 0;
                    time3 = 0;
                    time4 = 0;
                }
            }//End go through all spectra
         } catch(Exception e){
            LOG.error("Exception in Spectrum iterator ",e);
            e.printStackTrace();
        } catch(Throwable t){
            LOG.error("Throwable in Spectrum iterator ",t);
            t.printStackTrace();
        }
        finally {
            LOG.info("Finally  "+writer.loggerName()+" close ");
            writer.close();
        }

    }
//String.valueOf(timsFrame.getPrecursorCollisionEnergy(precursorId)),
    private void fillmzDBPrecursor(Precursor mzdbPrecursor, fr.profi.brucker.timstof.model.Precursor timstofPrecursor, String collEnergy ){
        scala.Option noneOp = scala.None$.MODULE$;

        IsolationWindowParamTree isolationParams = new IsolationWindowParamTree();
        List<CVParam> isolationParamsList = new ArrayList<>();
        isolationParamsList.add(new CVParam(PsiMsCV.ISOLATION_WINDOW_TARGET_MZ().getAccession(), "isolation window target m/z", timstofPrecursor.getMonoIsotopicMz().toString(), "MS", Some.apply("MS"), Some.apply("MS:1000040"), Some.apply("m/z")));
        isolationParams.setCVParams(JavaConverters.asScalaIteratorConverter(isolationParamsList.iterator()).asScala().toSeq());
        mzdbPrecursor.isolationWindow_$eq(isolationParams);

        Activation activation = new Activation();
        List<CVParam> params = new ArrayList<>();
        //<cvParam cvRef="MS" accession="MS:1000133" name="collision-induced dissociation" value=""/>
        params.add(new CVParam("MS:1000133", "collision-induced dissociation", "", "MS", noneOp, noneOp, noneOp));
        //<cvParam cvRef="MS" accession="MS:1000045" name="collision energy" value="30.0" unitCvRef="UO" unitAccession="UO:0000266" unitName="electronvolt"/>
        params.add(new CVParam("MS:1000045", "collision energy", collEnergy, "MS", Some.apply("UO"), Some.apply("UO:0000266"), Some.apply("electronvolt")));
        activation.setCVParams(JavaConverters.asScalaIteratorConverter(params.iterator()).asScala().toSeq());
        mzdbPrecursor.activation_$eq(activation);

        SelectedIonList ionList = new SelectedIonList();
        ionList.count_$eq(1);
        SelectedIon ion = new SelectedIon();
        List<SelectedIon> selectedIons = new ArrayList<>();
        List<CVParam> ionParams = new ArrayList<>();
        ionParams.add(new CVParam(PsiMsCV.SELECTED_ION_MZ().getAccession(), "selected ion m/z", timstofPrecursor.getMonoIsotopicMz().toString(), "MS", Some.apply("MS"), Some.apply("MS:1000040"), Some.apply("m/z")));
        ionParams.add(new CVParam("MS:1000041", "charge state", timstofPrecursor.getCharge().toString(), "MS", noneOp, noneOp, noneOp));
        ionParams.add(new CVParam("MS:1000042", "peak intensity", timstofPrecursor.getIntensity().toString(), "MS", Some.apply("MS"), Some.apply("MS:1000131"), Some.apply("number of counts")));
        ion.setCVParams(JavaConverters.asScalaIteratorConverter(ionParams.iterator()).asScala().toSeq());
        selectedIons.add(ion);
        ionList.selectedIons_$eq(JavaConverters.asScalaIteratorConverter(selectedIons.iterator()).asScala().toSeq());
        mzdbPrecursor.selectedIonList_$eq(ionList);
    }

    public static void main(String[] args) {
        try {
            File ttDir = new File(filepath);
            TimstofReader ttreader = TimstofReader.getTimstofReader();
            Long hdler = ttreader.openTimstofFile(ttDir);
            Timstof2Mzdb inst = new Timstof2Mzdb(ttDir, hdler, ttreader);
            inst.createMZdBData();
            LOG.info("close file."  );
            ttreader.closeTimstofFile(hdler);


        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
