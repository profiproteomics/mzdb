package fr.profi.mzdb.io.writer;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;
import fr.profi.mzdb.BBSizes;
import fr.profi.mzdb.db.model.*;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.db.model.params.param.*;
import fr.profi.mzdb.db.table.*;
import fr.profi.mzdb.model.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.ByteBuffer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class MzDBWriter {

  final static Logger logger = LoggerFactory.getLogger(MzDBWriter.class);

  public static double MODEL_VERSION = 0.7;

  File dbLocation;
  MzDBMetaData metaData;
  BBSizes bbSizes;
  boolean isDIA;

  private SQLiteConnection sqliteConnection = null;
  private SQLiteStatement bboxInsertStmt;
  private SQLiteStatement rtreeInsertStmt;
  private SQLiteStatement msnRtreeInsertStmt;
  private SQLiteStatement spectrumInsertStmt;

  private DataEncodingRegistry dataEncodingRegistry;
  private BoundingBoxCache bbCache;

  private List<SpectrumToWrite> spectrumCache = new ArrayList<>();
  private RunSliceStructureFactory runSliceStructureFactory;
  private long insertedSpectraCount;


  protected final StopWatch globalSW = new StopWatch("Global");
  protected final StopWatch insertSpectrumSW = new StopWatch("InsertSpectrum");
  protected final StopWatch metadataSW = new StopWatch("MetaData");


  public MzDBWriter(File location, MzDBMetaData metaData, BBSizes bbSizes, Boolean isDIA) {
    this.dbLocation = location;
    this.metaData = metaData;
    this.bbSizes = bbSizes;
    this.isDIA = isDIA;
    dataEncodingRegistry = new DataEncodingRegistry();
    runSliceStructureFactory = new RunSliceStructureFactory(1, 1000, bbSizes);
    insertedSpectraCount = 0;
    bbCache = new BoundingBoxCache(bbSizes);
    globalSW.start();
    insertSpectrumSW.start();
    insertSpectrumSW.suspend();
    metadataSW.start();
    metadataSW.suspend();
  }

  public void initialize() throws SQLiteException {
    logger.debug("Initialize Writer for "+dbLocation.getName());
    createConnection();
    insertMetaData();
  }

  private void createConnection() throws SQLiteException {
    this.sqliteConnection = new SQLiteConnection(dbLocation);
    sqliteConnection.open(true); //Allow with allow create = truec
    // See: https://blog.devart.com/increasing-sqlite-performance.html
    sqliteConnection.exec("PRAGMA encoding='UTF-8';");
    sqliteConnection.exec("PRAGMA synchronous=OFF;");
    sqliteConnection.exec("PRAGMA journal_mode=OFF;");
    sqliteConnection.exec("PRAGMA temp_store=2;");
    sqliteConnection.exec("PRAGMA cache_size=-100000;") ;// around 100 Mo
    sqliteConnection.exec("PRAGMA page_size=4096;"); // see: https://www.sqlite.org/pgszchng2016.html

    sqliteConnection.exec("PRAGMA automatic_index=OFF;");
    sqliteConnection.exec("PRAGMA locking_mode=EXCLUSIVE;") ;// we want to lock file access for the whole creation process

    sqliteConnection.exec("PRAGMA foreign_keys=OFF;") ;
    sqliteConnection.exec("PRAGMA ignore_check_constraints=ON;"); // to be a little bit faster (should be OFF in dev mode)

    // BEGIN TRANSACTION
    sqliteConnection.exec("BEGIN TRANSACTION;");

    // Init DDL schema
    sqliteConnection.exec(MzDBSchema.getSchemaDDL());

    // Init some INSERT statements //
    bboxInsertStmt = sqliteConnection.prepare("INSERT INTO "+ BoundingBoxTable.tableName+" VALUES (NULL, ?, ?, ?, ?)",  false);
    rtreeInsertStmt = sqliteConnection.prepare("INSERT INTO bounding_box_rtree VALUES (?, ?, ?, ?, ?)", false);
    msnRtreeInsertStmt = sqliteConnection.prepare("INSERT INTO bounding_box_msn_rtree VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",  false);

    StringBuilder placeHolders = new StringBuilder();
    for(int i =0; i<24; i++)
      placeHolders.append("?, ");
    placeHolders.delete(placeHolders.length()-2, placeHolders.length()-1);
    spectrumInsertStmt = sqliteConnection.prepare("INSERT INTO spectrum VALUES ("+placeHolders+")",  false);

  }

  private void insertMetaData() throws SQLiteException {
    metadataSW.resume();
    if (this.sqliteConnection == null)
      throw new IllegalStateException("The database connection isn't initialized  !");
    logger.debug(" --- insertMetaData ");

    // --- INSERT DATA PROCESSING --- //
    logger.trace("     - INSERT DATA PROCESSING ");
    SQLiteStatement stmt = sqliteConnection.prepare("INSERT INTO "+ DataProcessingTable.tableName+" VALUES (NULL, ?)", false);
    List<ProcessingMethod> procMethods = metaData.getProcessingMethods();
    List<String> dpNames = procMethods.stream().map(ProcessingMethod::getDataProcessingName).distinct().collect(Collectors.toList());
    logger.trace("     --- NBR "+dpNames.size());
    Map<String, Long> dpIdByName = new HashMap<>();
    for(String dpName : dpNames){
      stmt.bind(1, dpName);
      stmt.step();
      Long dpId = sqliteConnection.getLastInsertId();
      stmt.reset();
      dpIdByName.put(dpName, dpId);
    }
    stmt.dispose();

    // --- INSERT PROCESSING METHODS --- //
    logger.trace("     - INSERT PROCESSING METHODS ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ ProcessingMethodTable.tableName+" VALUES (NULL, ?, ?, ?, ?, ?)", false);
    logger.trace("     --- NBR "+procMethods.size());
    for (ProcessingMethod procMethod : procMethods) {
      stmt.bind(1, procMethod.getNumber());
      if (!procMethod.hasParamTree())
        stmt.bind(2, ParamTreeStringifier.stringifyParamTree(new ParamTree()));
      else
        stmt.bind(2, ParamTreeStringifier.stringifyParamTree(procMethod.getParamTree(sqliteConnection)));
      stmt.bindNull(3);
      stmt.bind(4, dpIdByName.get(procMethod.getDataProcessingName()));
      stmt.bind(5, procMethod.getSoftwareId());
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    // --- INSERT CV/CVTerm/CVUnit METHODS --- //
    logger.trace("     - INSERT CV** METHODS ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ CvTable.tableName+" VALUES (?, ?, ?, ?)", false);
    List<CV> cvs  = metaData.getCvList();
    logger.trace("     --- NBR CV "+cvs.size());
    for (CV nextCV : cvs) {
      stmt.bind(1, nextCV.getCvId());
      stmt.bind(2, nextCV.getFullName());
      stmt.bind(3, nextCV.getCvVersion());
      stmt.bind(4, nextCV.getUri());
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    stmt = sqliteConnection.prepare("INSERT INTO "+ CVTermTable.tableName+" VALUES (?, ?, ?, ?)", false);
    List<CVTerm> cvTerms  = metaData.getCvTerms();
    logger.trace("     --- NBR CV Terms "+cvTerms.size());
    for (CVTerm nextCVTerm : cvTerms) {
      stmt.bind(1, nextCVTerm.getAccession());
      stmt.bind(2, nextCVTerm.getName());
      stmt.bind(3, nextCVTerm.getUnitAccession());
      stmt.bind(4, nextCVTerm.getCvId());
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    stmt = sqliteConnection.prepare("INSERT INTO "+ CvUnitTable.tableName+" VALUES (?, ?, ?)", false);
    List<CVUnit> cvUnits  = metaData.getCvUnits();
    logger.trace("     --- NBR CV Units "+cvUnits.size());
    for (CVUnit nextCVUnit : cvUnits) {
      stmt.bind(1, nextCVUnit.getAccession());
      stmt.bind(2, nextCVUnit.getName());
      stmt.bind(3, nextCVUnit.getCvId());
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    // --- INSERT SHARED PARAM TREES --- //
    logger.trace("     - INSERT SHARED PARAM ");
    List<SharedParamTree> shParamTrees = metaData.getSharedParamTrees();
    shParamTrees.sort((o1, o2) -> {
      if(o1 == null)
        return -1;
      if(o2 == null)
        return 1;
      return Long.compare(o1.getId(), o2.getId());
    });

    logger.trace("       -- NBR "+shParamTrees.size());
    stmt = sqliteConnection.prepare("INSERT INTO " + SharedParamTreeTable.tableName + " VALUES (NULL, ?, ?)", false);
    for (SharedParamTree sharedParamTree :  shParamTrees){
      stmt.bind(1, ParamTreeStringifier.stringifyRefParamGroup(sharedParamTree.getData()));
      stmt.bind(2, sharedParamTree.getSchemaName());
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    // --- INSERT INSTRUMENT CONFIGS --- //
    logger.trace("     - INSERT INSTRUMENT CONFIGS ");
    stmt = sqliteConnection.prepare("INSERT INTO " + InstrumentConfigurationTable.tableName + " VALUES (NULL, ?, NULL, ?, NULL,  ?)", false);
    List<InstrumentConfiguration> instConfigs = metaData.getInstrumentConfigurations();
    logger.trace("     --- NBR "+instConfigs.size());
    for (InstrumentConfiguration instConfig :  instConfigs){
      if (instConfig.getComponentList() != null) {
        stmt.bind(1, instConfig.getName());
        stmt.bind(2, ParamTreeStringifier.stringifyComponentList(instConfig.getComponentList()));
//        if(instConfig.getSoftwareId() == null)
//          stmt.bindNull(3);
//        else
          stmt.bind(3, instConfig.getSoftwareId());
        stmt.step();
        stmt.reset();
      }
    }
      stmt.dispose();

    // --- INSERT MZDB HEADER --- //
    logger.trace("     - INSERT MZDB HEADER ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ MzdbTable.tableName+" VALUES (?, ?, ?, ?, ?)", false);
    MzDbHeader mzDbHeader = metaData.getMzdbHeader();

    /*
    // SMALL TEMP HACK: change BB sizes because we currently store each spectrum in a single BB
    // FIXME: remove this hack
    var mzdbHeaderParams = mzDbHeader.getParamTree()
    val patchedUserParams = mzdbHeaderParams.getUserParams().map { userParam =>
      if (userParam.name == "ms1_bb_mz_width") userParam.copy(value = "10000")
      else if (userParam.name == "ms1_bb_time_width") userParam.copy(value = "0")
      else userParam
    }
    mzdbHeaderParams = mzdbHeaderParams.copy(userParams = patchedUserParams)
    */
//
    // Update BB sizes in params
    ParamTree mzdbHeaderParams = mzDbHeader.hasParamTree() ? mzDbHeader.getParamTree(sqliteConnection) : new ParamTree();
    HashSet<String> bbSizesKeySet = new HashSet<>();
    bbSizesKeySet.add("ms1_bb_mz_width");
    bbSizesKeySet.add("ms1_bb_time_width");
    bbSizesKeySet.add("msn_bb_mz_width");
    bbSizesKeySet.add("msn_bb_time_width");

    List<UserParam> userExtraParams = mzdbHeaderParams.getUserParams().stream().filter(p -> !bbSizesKeySet.contains(p.getName()) ).collect(Collectors.toList());

    userExtraParams.add(new UserParam(null, null,"ms1_bb_mz_width", String.valueOf(bbSizes.BB_MZ_HEIGHT_MS1), "xsd:float"));
    userExtraParams.add(new UserParam(null, null, "ms1_bb_time_width", String.valueOf(bbSizes.BB_RT_WIDTH_MS1),  "xsd:float"));
    userExtraParams.add(new UserParam(null, null, "msn_bb_mz_width", String.valueOf(bbSizes.BB_MZ_HEIGHT_MSn),  "xsd:float"));
    userExtraParams.add(new UserParam(null, null, "msn_bb_time_width", String.valueOf(bbSizes.BB_RT_WIDTH_MSn),  "xsd:float"));

    /*val patchedUserParams = mzdbHeaderParams.getUserParams().map { userParam =>
      userParam.name match {
        case "ms1_bb_mz_width" => userParam.copy(value = bbSizes.BB_MZ_HEIGHT_MS1.toString)
        case "ms1_bb_time_width" => userParam.copy(value = bbSizes.BB_RT_WIDTH_MS1.toString)
        case "msn_bb_mz_width" => userParam.copy(value = bbSizes.BB_MZ_HEIGHT_MSn.toString)
        case "msn_bb_time_width" => userParam.copy(value = bbSizes.BB_MZ_HEIGHT_MSn.toString)
        case _ => userParam
      }
    }*/

    mzdbHeaderParams.setUserParams(userExtraParams);
    String serializedMzDbParamTree = ParamTreeStringifier.stringifyParamTree(mzdbHeaderParams);

    stmt.bind(1, MODEL_VERSION);
    stmt.bind(2, Integer.parseInt( String.valueOf(new java.util.Date().getTime() / 1000)));
    stmt.bind(3, ParamTreeStringifier.stringifyFileContentParam(mzDbHeader.getFileContent()));
    stmt.bind(4, ""); // FIXME: define contacts in the mzDB file
    stmt.bind(5, serializedMzDbParamTree);

    stmt.step();
    stmt.reset();
    stmt.dispose();

    // --- INSERT RUNS --- //
    logger.trace("     - INSERT RUNS ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ RunTable.tableName+" VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?)", false);
    List<Run> runs = metaData.getRuns();
    logger.trace("     --- NBR "+runs.size());
    DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.US);
    for (Run run : runs) {
      // Inject the 'acquisition parameter' CV param if it doesn't exist
      ParamTree runParamTree = run.hasParamTree() ? run.getParamTree(sqliteConnection) : new ParamTree();
      List<CVParam> cvParams = runParamTree.getCVParams();
      long nbr = cvParams.stream().filter(cv -> cv.getAccession().equals(CVEntry.ACQUISITION_PARAMETER.getAccession())).count();
      if(nbr == 0){
        String modeName = isDIA ? AcquisitionMode.SWATH.name() : AcquisitionMode.DDA.name();
        CVParam cvParam = new CVParam();
        cvParam.setCvRef("MS");
        cvParam.setAccession(CVEntry.ACQUISITION_PARAMETER.getAccession());
        cvParam.setName("acquisition parameter");
        cvParam.setValue(modeName);
        cvParams.add(cvParam);
        runParamTree.setCvParams(cvParams);
      }

      stmt.bind(1, run.getName());
      stmt.bind(2, dateFormat.format(run.getStartTimestamp()));
      stmt.bind(3, ParamTreeStringifier.stringifyParamTree(runParamTree));
      // FIXME: do not use default values
      if (metaData.getSharedParamTrees() == null || metaData.getSharedParamTrees().isEmpty()) {
        stmt.bindNull(4);
      } else {
        // TODO do not suppose that there is only one SharedParam
        stmt.bind(4, 1);
      }
      stmt.bind(5, 1);
      stmt.bind(6, 1);
      stmt.bind(7, 1);
      stmt.bind(8, 1);
      stmt.bind(9, 1);
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    // --- INSERT SOURCE FILES --- //
    logger.trace("     - INSERT SOURCE FILES ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ SourceFileTable.tableName+" VALUES (NULL, ?, ?, ?, NULL)", false);
    List<SourceFile> sourceFiles = metaData.getSourceFiles();
    logger.trace("         - Nbr "+sourceFiles.size());
    for (SourceFile sourceFile: sourceFiles) {
      stmt.bind(1, sourceFile.getName());
      stmt.bind(2, sourceFile.getLocation());
      // FIXME: source file paramtree should be defined
      if (sourceFile.hasParamTree()) {
        stmt.bind(3, ParamTreeStringifier.stringifyParamTree(sourceFile.getParamTree(sqliteConnection)));
      } else {
        stmt.bind(3, "");
      }
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();

    // --- INSERT SAMPLES --- //
    logger.trace("     - INSERT SAMPLES ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ SampleTable.tableName+" VALUES (NULL, ?, ?, NULL)", false);

    List<Sample> samples = metaData.getSamples();
    logger.trace("         - Nbr "+samples.size());
    if(samples.isEmpty()) {
      String sampleName;
      if(sourceFiles.isEmpty())
        sampleName= dbLocation.getName().split("\\.")[0];
      else
        sampleName = sourceFiles.get(0).getName();
      stmt.bind(1, sampleName);
      stmt.step();
    } else {
      for (Sample sample : samples) {
        stmt.bind(1, sample.getName());
        if (!sample.hasParamTree())
          stmt.bindNull(2);
        else
          stmt.bind(2, ParamTreeStringifier.stringifyParamTree(sample.getParamTree(sqliteConnection)));
        stmt.step();
        stmt.reset();
      }
    }
    stmt.dispose();

    // --- INSERT SOFTWARE LIST --- //
    logger.trace("     - INSERT SOFTWARE LIST ");
    stmt = sqliteConnection.prepare("INSERT INTO "+ SoftwareTable.tableName+" VALUES (NULL, ?, ?, ?, NULL)", false);
    List<Software> softwareList = metaData.getSoftwares();
    logger.trace("         - Nbr "+softwareList.size());
    for (Software software : softwareList) {
      stmt.bind(1, software.getName());
      stmt.bind(2, software.getVersion());
      // FIXME: software paramtree should be defined
      ParamTree pt = software.hasParamTree() ? software.getParamTree(sqliteConnection) :  new ParamTree();
      stmt.bind(3, ParamTreeStringifier.stringifyParamTree(pt));
      stmt.step();
      stmt.reset();
    }
    stmt.dispose();
    metadataSW.stop();
    logger.trace("  --- Insert MetaData done ");
  }

 public void close(){
    if (this.sqliteConnection == null)
      throw new IllegalStateException("The method open() must first be called");
    try {

      // FIXME: insert missing BBs (last entries in bbCache)
      for(Pair<Integer, IsolationWindow> p : this.bbCache.getBBRowsKeys()) {
         Long bbFirstSpectrumId = flushBBRow(p.getKey(), p.getValue());
         if (bbFirstSpectrumId != null) flushSpectrum(p.getKey(), p.getValue(), bbFirstSpectrumId);
       }


      try {

        // --- INSERT DATA ENCODINGS --- //
        SQLiteStatement stmt = sqliteConnection.prepare("INSERT INTO "+ DataEncodingTable.tableName+" VALUES (?, ?, ?, ?, ?, ?, NULL)", false);
        List<DataEncoding> dataEncs = dataEncodingRegistry.getDistinctDataEncoding();
        for (DataEncoding dataEnc : dataEncs) {
          long mzPrecision = 64;
          long intPrecision = 32;
          PeakEncoding peakEnc = dataEnc.getPeakEncoding();

          if (peakEnc == PeakEncoding.LOW_RES_PEAK)
            mzPrecision = 32;
          else if (peakEnc == PeakEncoding.NO_LOSS_PEAK)
            intPrecision = 64;

          stmt.bind(1, dataEnc.getId());
          stmt.bind(2, dataEnc.getMode().name());
          stmt.bind(3, dataEnc.getCompression());
          stmt.bind(4, dataEnc.getByteOrder().toString().toLowerCase());
          stmt.bind(5, mzPrecision);
          stmt.bind(6, intPrecision);
          stmt.step();
          stmt.reset();
        }
        stmt.dispose();

        // Finalize the creation of run slices
        stmt = sqliteConnection.prepare("INSERT INTO "+ RunSliceTable.tableName+" VALUES (?, ?, ?, ?, ?, NULL, ?)", false);
        for (RunSliceHeader runSlice :  this.runSliceStructureFactory.getAllRunSlices()) {
          stmt.bind(1, runSlice.getId());
          stmt.bind(2, runSlice.getMsLevel());
          stmt.bind(3, runSlice.getNumber());
          stmt.bind(4, runSlice.getBeginMz());
          stmt.bind(5, runSlice.getEndMz());
          stmt.bind(6, runSlice.getRunId());

          stmt.step();
          stmt.reset();
        }
        stmt.dispose();

        // Create all indexes here
        this.sqliteConnection.exec("CREATE UNIQUE INDEX spectrum_initial_id_idx ON spectrum (initial_id ASC,run_id ASC);");
        this.sqliteConnection.exec("CREATE INDEX spectrum_ms_level_idx ON spectrum (ms_level ASC,run_id ASC);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX run_name_idx ON run (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX run_slice_mz_range_idx ON run_slice (begin_mz ASC,end_mz ASC,ms_level ASC,run_id ASC);");
        this.sqliteConnection.exec("CREATE INDEX bounding_box_run_slice_idx ON bounding_box (run_slice_id ASC);");
        this.sqliteConnection.exec("CREATE INDEX bounding_box_first_spectrum_idx ON bounding_box (first_spectrum_id ASC); ");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX controlled_vocabulary_full_name_idx ON cv (full_name);");
        this.sqliteConnection.exec("CREATE INDEX controlled_vocabulary_uri_idx ON cv (uri);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX source_file_name_idx ON source_file (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX sample_name_idx ON sample (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX software_name_idx ON software (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX instrument_configuration_name_idx ON instrument_configuration (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX processing_method_number_idx ON processing_method (number ASC);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX data_processing_name_idx ON data_processing (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX chromatogram_name_idx ON chromatogram (name);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX cv_term_name_idx ON cv_term (name ASC);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX user_term_name_idx ON user_term (name ASC);");
        this.sqliteConnection.exec("CREATE UNIQUE INDEX cv_unit_name_idx ON cv_unit (name ASC);");
        this.sqliteConnection.exec("CREATE INDEX spectrum_bb_first_spectrum_id_idx ON spectrum (bb_first_spectrum_id ASC);");

        // COMMIT TRANSACTION
        this.sqliteConnection.exec("COMMIT TRANSACTION;");

      } finally {
        if (this.bboxInsertStmt != null) bboxInsertStmt.dispose();
        if (this.rtreeInsertStmt != null) rtreeInsertStmt.dispose();
        if (this.msnRtreeInsertStmt != null) msnRtreeInsertStmt.dispose();
        if (this.spectrumInsertStmt != null) spectrumInsertStmt.dispose();
        this.sqliteConnection.dispose();
      }

      } catch (SQLiteException e) {
        e.printStackTrace();
      }

    globalSW.stop();
    insertSpectrumSW.stop();
    logger.debug("mzDB Writer closed");
    }

  /*
   */
  public void insertSpectrum(Spectrum spectrum, SpectrumMetaData metaDataAsText, DataEncoding dataEncoding) throws SQLiteException {

    insertSpectrumSW.resume();
    SpectrumHeader sh = spectrum.getHeader();
    SpectrumData sd = spectrum.getData();
    int peaksCount = sd.getPeaksCount();

    // FIXME: deal with empty spectra
    if (peaksCount == 0) {
      logger.warn("Empty spectrum detected !!! no insertion in the mzdb file");
      return;
    }
    insertedSpectraCount += 1;

    int msLevel = sh.getMsLevel();
    IsolationWindow isolationWindowOpt = (isDIA && msLevel == 2) ?  sh.getIsolationWindow() : null; // very important for cache
    long spectrumId = insertedSpectraCount;  // note: we maintain our own spectrum ID counter
    Float spectrumTime = sh.getElutionTime();

    DataEncoding dataEnc = this.dataEncodingRegistry.getOrAddDataEncoding(dataEncoding);
    Double mzInc = (msLevel == 1) ? bbSizes.BB_MZ_HEIGHT_MS1 :  bbSizes.BB_MZ_HEIGHT_MSn;


      // FIXME: min m/z should be retrieve from meta-data (scan list)
      float curMinMz = (float) ((Math.floor(sd.getMzList()[0] / bbSizes.BB_MZ_HEIGHT_MS1)) * bbSizes.BB_MZ_HEIGHT_MS1);
      float curMaxMz = (float) (curMinMz + mzInc);

      // FIXME: this is a workaround => find a better way to do this
      if (msLevel == 2 && !isDIA) {
        curMinMz = 0;
        curMaxMz = (float)bbSizes.BB_MZ_HEIGHT_MSn;
      }

      boolean isTimeForNewBBRow = bbCache.isTimeForNewBBRow(msLevel, isolationWindowOpt, spectrumTime);

      // Flush BB row when we reach a new row (retention time exceeding size of the bounding box for this MS level)
      if (isTimeForNewBBRow) {
        //println("******************************************************* FLUSHING BB ROW ****************************************")
        Long bbFirstSpectrumId = flushBBRow(msLevel, isolationWindowOpt);
        if (bbFirstSpectrumId != null) flushSpectrum(msLevel, isolationWindowOpt, bbFirstSpectrumId);
      }

      // TODO: put _getBBWithNextSpectrumSlice back here when memory issues are fixed

      // Peaks lookup to create Bounding Boxes
      int i = 0;
      BoundingBoxToWrite curBB = null;

      while (i < peaksCount) {
        double mz = sd.getMzList()[i];

        if (i == 0) {
          curBB = getBBWithNextSpectrumSlice(spectrum,spectrumId,spectrumTime,msLevel,dataEnc,isolationWindowOpt, i, curMinMz, curMaxMz);
        } else if (mz > curMaxMz) {
          // Creates new bounding boxes even for empty data => should be removed in mzDB V2
          while (mz > curMaxMz) {
            curMinMz += mzInc;
            curMaxMz += mzInc;
            // Very important: ensure run slices are created in increasing m/z order
            if (!runSliceStructureFactory.hasRunSlice(msLevel, curMinMz, curMaxMz))
              runSliceStructureFactory.addRunSlice(msLevel, curMinMz, curMaxMz);
          }
          curBB = getBBWithNextSpectrumSlice(spectrum,spectrumId,spectrumTime,msLevel,dataEnc,isolationWindowOpt, i, curMinMz, curMaxMz);
        }

        if (curBB.spectrumSlices.get(curBB.getSpectrumIds().size()-1) != null ) {
            // Add data point to the Bounding Box
            SpectrumSliceIndex lastSpectrumSlice = curBB.getSpectrumSlices().get(curBB.getSpectrumIds().size()-1);
            lastSpectrumSlice.setLastPeakIdx(i);
        }

        i++;
      }

    // --- INSERT SPECTRUM HEADER --- //

    spectrumCache.add(new SpectrumToWrite(sh, metaDataAsText, spectrumId, dataEnc));

    insertSpectrumSW.suspend();
  } // ends insertSpectrum

  private void flushSpectrum(int msLevel, IsolationWindow isolationWindow, Long bbFirstSpectrumId) throws SQLiteException {

    if (isolationWindow != null) {
      throw new UnsupportedOperationException("Not Yet implemented");
    }

    List<SpectrumToWrite> newSpectrumCache = new ArrayList<>();

    for (SpectrumToWrite s : spectrumCache) {
      if (s.header.getMsLevel() == msLevel) {
        insertSpectrumToWrite(s, bbFirstSpectrumId);
      } else {
        newSpectrumCache.add(s);
      }
    }

    spectrumCache = newSpectrumCache ;
  }

  private void insertSpectrumToWrite(SpectrumToWrite spectrumToWrite, Long bbFirstSpectrumId) throws SQLiteException {

    SQLiteStatement stmt = spectrumInsertStmt;
    stmt.bind(1, spectrumToWrite.spectrumId);

    // FIXME: Proline should be able to work with files where the initialID differs from the mzDB ID, thus sh.getInitialId() should be used when it is fixed
    stmt.bind(2, spectrumToWrite.spectrumId); // sh.getInitialId
    stmt.bind(3, spectrumToWrite.header.getTitle());
    stmt.bind(4, spectrumToWrite.header.getCycle());
    stmt.bind(5, spectrumToWrite.header.getTime());
    stmt.bind(6, spectrumToWrite.header.getMsLevel());

    if (spectrumToWrite.header.getActivationType() == null)
      stmt.bindNull(7);
    else
      stmt.bind(7, spectrumToWrite.header.getActivationType().name());

    stmt.bind(8, spectrumToWrite.header.getTIC());
    stmt.bind(9, spectrumToWrite.header.getBasePeakMz());
    stmt.bind(10, spectrumToWrite.header.getBasePeakIntensity());

    // Precursor information
    if (spectrumToWrite.header.getPrecursorMz() == null || spectrumToWrite.header.getPrecursorMz() == 0.0)
      stmt.bindNull(11);
    else
      stmt.bind(11, spectrumToWrite.header.getPrecursorMz());
    if (spectrumToWrite.header.getPrecursorCharge() == null || spectrumToWrite.header.getPrecursorCharge() ==  0)
      stmt.bindNull(12);
    else
      stmt.bind(12, spectrumToWrite.header.getPrecursorCharge());

    stmt.bind(13, spectrumToWrite.header.getPeaksCount());

    // XML Meta-data bindings
    stmt.bind(14, spectrumToWrite.metadata.getParamTree());
    stmt.bind(15, spectrumToWrite.metadata.getScanList());

    if (StringUtils.isEmpty(spectrumToWrite.metadata.getPrecursorList()))
      stmt.bindNull(16);
    else {
      String precList = spectrumToWrite.metadata.getPrecursorList();
      stmt.bind(16, precList);
    }

    stmt.bindNull(17); // No pruduct list
//      if (metaDataAsText.productList.isEmpty) stmt.bindNull(17)
//      else stmt.bind(17, smd.productList.get)

    stmt.bind(18, 1);
    stmt.bind(19, 1);
    stmt.bind(20, 1);
    stmt.bind(21, 1);
    stmt.bind(22, 1);
    stmt.bind(23, spectrumToWrite.dataEncoding.getId());
    stmt.bind(24, bbFirstSpectrumId);

    stmt.step();
    stmt.reset();
  }

  private BoundingBoxToWrite getBBWithNextSpectrumSlice(Spectrum spectrum, Long spectrumId, Float spectrumTime, Integer msLevel, DataEncoding dataEnc, IsolationWindow isolationWindow, Integer peakIdx, Float minMz, Float maxMz) {

//    val runSliceBoundaries = (msLevel, minMz, maxMz)
    Integer runSliceId = runSliceStructureFactory.getRunSliceId(msLevel, minMz, maxMz);
    if(runSliceId == null) {
      runSliceId =  runSliceStructureFactory.addRunSlice(msLevel, minMz, maxMz).getId();
    }

    BoundingBoxToWrite cachedBB = bbCache.getCachedBoundingBox(runSliceId, isolationWindow);

    if (cachedBB == null ) { // isTimeForNewBBRow ||
      // FIXME: perform this estimation by counting the number of run slices per MS level
      int sliceSlicesCountHint = (msLevel == 2) ? 1 : runSliceStructureFactory.getRunSlicesCount();
      cachedBB = bbCache.createBoundingBox(spectrumTime, runSliceId, msLevel, dataEnc, isolationWindow, sliceSlicesCountHint);
    } else {
      // Increase size of the bounding box for one new spectrum
      cachedBB.lastTime = spectrumTime ;// update BB last RT
    }

    cachedBB.getSpectrumIds().add(spectrumId);
    //bb.spectrumSlices += Some(_bbCache.acquireSpectrumDataBuilder(sh, sh.peaksCount))

    cachedBB.spectrumSlices.add(new SpectrumSliceIndex(spectrum.getData(), peakIdx, peakIdx));
    return  cachedBB;
  }

  private Long flushBBRow(Integer msLevel, IsolationWindow isolationWindowOpt)  {

    // Retrieve the longest list of spectra ids
    //val lcCtxBySpecId = new LongMap[ILcContext]()
    ArrayList<Long> spectraIds = new ArrayList<>();

    Function<BoundingBoxToWrite, Void> function = bb -> {
      spectraIds.addAll( bb.getSpectrumIds());
      return null;
    };
    bbCache.forEachCachedBB(msLevel, isolationWindowOpt, function);

    List<Long> distinctBBRowSpectraIds = spectraIds.stream().distinct().sorted().collect(Collectors.toList());

    // Insert all BBs corresponding to the same MS level and the same isolation window (DIA only)
    Function<BoundingBoxToWrite, Void> func2 = bb -> {
      // Map slices by spectrum id
      HashMap<Long, SpectrumSliceIndex> specSliceById = new HashMap<>();
      int spSize= bb.getSpectrumIds().size();
      for (int i=0; i<spSize; i++){
        if(bb.spectrumSlices.get(i)!=null){
          specSliceById.put(bb.spectrumIds.get(i), bb.spectrumSlices.get(i));
        }
      }


      int distinctSpSize= distinctBBRowSpectraIds.size();
      List<SpectrumSliceIndex> distinctBBSpectraSlice = new ArrayList<>(distinctSpSize);
      for (int i=0; i<distinctSpSize; i++){
        distinctBBSpectraSlice.add(i,specSliceById.get(distinctBBRowSpectraIds.get(i)));
      }

      // Update Bounding Box
      bb.spectrumIds.clear();
      bb.spectrumIds.addAll(distinctBBRowSpectraIds);

      bb.spectrumSlices.clear();
      bb.spectrumSlices.addAll(distinctBBSpectraSlice);

      // Insert Bounding Box
      try {
        this.insertAndIndexBoundingBox(bb);
      } catch (SQLiteException e) {
        e.printStackTrace();
        throw new RuntimeException(e);
      }
      return null;
    };

    bbCache.forEachCachedBB(msLevel, isolationWindowOpt, func2);
    bbCache.removeBBRow(msLevel, isolationWindowOpt);

    return distinctBBRowSpectraIds.isEmpty() ? null : distinctBBRowSpectraIds.get(0);
  }

  private void insertAndIndexBoundingBox(BoundingBoxToWrite bb) throws SQLiteException { // --- INSERT BOUNDING BOX --- //

    Integer msLevel = bb.getMsLevel();
    long bbId = this.insertBoundingBoxToWrite(bb);

    RunSliceHeader runSlice = this.runSliceStructureFactory.getRunSlice(bb.runSliceId);

    // TODO: insert this index at the end of the file creation
    SQLiteStatement stmt = null;
    boolean isRTreeIndexInserted;
    try {
      if (msLevel == 1) {
        stmt = rtreeInsertStmt;

        stmt.bind(1, bbId);
        stmt.bind(2, runSlice.getBeginMz());
        stmt.bind(3, runSlice.getEndMz());
        stmt.bind(4, bb.getFirstTime());
        stmt.bind(5, bb.getLastTime());

        isRTreeIndexInserted = true;

      } else if (msLevel == 2 && isDIA) {

        // TODO: parse this in the MzMLParser?
        // TODO: remove this try/catch
        /*val precursorListStr = smd.precursorList.get
        val selectedIons = precursorListStr.split("selectedIon>")
        if (selectedIons.length <= 1) false
        else {
          var kv = Array.empty[String]
          val selectedIonMzOpt = selectedIons(1).split(" ").collectFirst { case s if { kv = s.split("="); kv(0) == "value" } => {
              val mzStr = kv(1)
              mzStr.substring(1, mzStr.length()-1).toDouble // remove quotes and convert to Double
            }
          }
          val selectedIonMz = selectedIonMzOpt.get

          val nearestWindowOpt = diaIsolationWindows.get.find { win: IsolationWindow =>
            win.minMz <= selectedIonMz && win.maxMz >= selectedIonMz
          }*/

        IsolationWindow isolationWindowOpt = bb.getIsolationWindow();

        if (isolationWindowOpt == null)
          isRTreeIndexInserted = false;
        else {
          stmt = msnRtreeInsertStmt;

          stmt.bind(1, bbId);
          stmt.bind(2, msLevel);
          stmt.bind(3, msLevel);
          stmt.bind(4, isolationWindowOpt.getMinMz());
          stmt.bind(5, isolationWindowOpt.getMaxMz());
          stmt.bind(6, runSlice.getBeginMz());
          stmt.bind(7, runSlice.getEndMz());
          stmt.bind(8, bb.getFirstTime());
          stmt.bind(9, bb.getLastTime());

          isRTreeIndexInserted = true;
        }
      } else
        isRTreeIndexInserted = false;
    } catch  (SQLiteException e) {
      e.printStackTrace();
      isRTreeIndexInserted = false;
    }


    if (isRTreeIndexInserted) {
      // execute R*Tree index insert statement
      try {
        stmt.step();
        stmt.reset();
      } catch (SQLiteException e) {
        e.printStackTrace();
      }
    } else if (isDIA) {
      System.out.println("No R*Tree index inserted for BB with id = "+bb.id+"; MS level = "+msLevel);
    }

  }
 protected Long insertBoundingBoxToWrite(BoundingBoxToWrite bb) throws SQLiteException {
   List<Long> spectrumIds = bb.getSpectrumIds();
   List<SpectrumSliceIndex> spectrumSlices = bb.getSpectrumSlices();
   int slicesCount = spectrumSlices.size();
   int bbPeaksCount = 0;
   for(SpectrumSliceIndex spectrumSliceIndex : spectrumSlices){
     if(spectrumSliceIndex != null)
      bbPeaksCount += spectrumSliceIndex.peaksCount();
   }
  logger.trace("BB "+bb.getId()+" has "+slicesCount+" slicesCount with nbr peaks "+bbPeaksCount);
   DataEncoding dataEnc = bb.getDataEncoding();
   PeakEncoding pe = dataEnc.getPeakEncoding();

   // Retrieve m/z and intensity data
   assert(pe != PeakEncoding.NO_LOSS_PEAK ) :  "The NO_LOSS_PEAK encoding is no yet supported!";

   int peakStructSize = dataEnc.getPeakStructSize();

   int bbLen = (int)(8L * slicesCount) + (peakStructSize * bbPeaksCount);

   byte[] bbBytes = new byte[bbLen];
   //val bytesBuffer = new scala.collection.mutable.ArrayBuffer[Byte](bbLen.toInt)
   ByteBuffer bytesBuffer = ByteBuffer.wrap(bbBytes).order(dataEnc.getByteOrder());

   // --- SERIALIZE SPECTRUM SLICES TO BOUNDING BOX BYTES --- //
   int sliceIdx = 0;
   while (sliceIdx < slicesCount) {

     Long spectrumId = spectrumIds.get(sliceIdx);
     bytesBuffer.putInt(spectrumId.intValue());

     SpectrumSliceIndex spectrumSliceIdx = spectrumSlices.get(sliceIdx);

     if (spectrumSliceIdx == null )
       bytesBuffer.putInt(0);
     else {

       SpectrumData spectrumData = spectrumSliceIdx.getSpectrumData();
       float firstPeakIdx = spectrumSliceIdx.getFirstPeakIdx();
       float lastPeakIdx = spectrumSliceIdx.getLastPeakIdx();

       int slicePeaksCount = spectrumSliceIdx.peaksCount();
       bytesBuffer.putInt(slicePeaksCount);

       int i = (int) firstPeakIdx;
       while (i <= lastPeakIdx) {

         if (pe == PeakEncoding.HIGH_RES_PEAK) {
           bytesBuffer.putDouble(spectrumData.getMzList()[i]);
         } else {
           bytesBuffer.putFloat(Double.valueOf(spectrumData.getMzList()[i]).floatValue());
         }

         bytesBuffer.putFloat(spectrumData.getIntensityList()[i]);

         if (dataEnc.getMode().equals(DataMode.FITTED) ){
           if(spectrumData.getLeftHwhmList() != null && spectrumData.getLeftHwhmList()[i] > 0.0f)
            bytesBuffer.putFloat(spectrumData.getLeftHwhmList()[i]);
           else
             bytesBuffer.putFloat(0f);

           if(spectrumData.getRightHwhmList() != null && spectrumData.getRightHwhmList()[i] > 0.0f)
            bytesBuffer.putFloat(spectrumData.getRightHwhmList()[i]);
           else
             bytesBuffer.putFloat(0f);
         }
// TODO use mzdb-access ion_mobility branch to enable this feature
//         if (dataEnc.getMode().equals(DataMode.CENTROID_3D)){
//           if (spectrumData.getMobilityIndexList() != null)
//             bytesBuffer.putShort(spectrumData.getMobilityIndexList()[i]);
//         }

         i += 1;
       }
     }

     sliceIdx += 1;
   }

   SQLiteStatement stmt = this.bboxInsertStmt; //.asInstanceOf[SQLiteStatementWrapper].stmt
   stmt.bind(1, bbBytes);
   stmt.bind(2, bb.getRunSliceId());
   stmt.bind(3, spectrumIds.get(0)); // first_spectrum_id
   stmt.bind(4, spectrumIds.get(spectrumIds.size()-1)); // last_spectrum_id
   stmt.step();

   long bbId = this.sqliteConnection.getLastInsertId();
   stmt.reset();

   return bbId;
 }

 public void dumpExecutionWatches() {
   logger.info("Global stop watch : "+globalSW.formatTime());
   logger.info("insertSpectrum stop watch : "+insertSpectrumSW.formatTime());
   logger.info("metadata stop watch : "+metadataSW.formatTime());

 }

}
