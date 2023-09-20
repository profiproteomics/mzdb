package fr.profi.mzdb.io.util;

import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.model.*;
import fr.profi.mzdb.db.model.params.param.CV;
import fr.profi.mzdb.db.model.params.param.CVTerm;
import fr.profi.mzdb.db.model.params.param.CVUnit;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.MzDBMetaData;
import fr.profi.mzdb.model.ProcessingMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MzDBUtil {


  private final static Logger LOG = LoggerFactory.getLogger(MzDBUtil.class);


  public static MzDBMetaData createMzDbMetaData(MzDbReader srcReader) throws SQLiteException {


//    ParamTree paramTree = new ParamTree();
//
//    List<UserParam> userParams = new ArrayList<>();
//    for (UserParam srcUserParam : srcReader.getMzDbHeader().getUserParams()) {
//      userParams.add(new UserParam(null, null, srcUserParam.getName(), srcUserParam.getValue(), srcUserParam.getType()));
//    }
//    paramTree.setUserParams(userParams);
//    int currentTime = Long.valueOf(System.currentTimeMillis()).intValue();
//    MzDbHeader mzdbHeader = new MzDbHeader(srcReader.getMzDbHeader().getVersion(), currentTime, paramTree, null);
    MzDbHeader mzdbHeader = srcReader.getMzDbHeader();

    List<DataEncoding> dataEncodings = new ArrayList<>(Arrays.asList(srcReader.getDataEncodingReader().getDataEncodings()));
    List<InstrumentConfiguration> instrumentConfigurations = srcReader.getInstrumentConfigurations();


    // Processing methods ... except that there is no api in mzdb-access to read Processing methods. TODO !!
    List<ProcessingMethod> processingMethods = new ArrayList<>();
    int processingMethodsNumber = 0;
    int processingMethodsid = 1;
    List<Software> softwares = srcReader.getSoftwareList();
    for (Software srcSoftware : softwares) {
      ProcessingMethod pm = new ProcessingMethod(processingMethodsid++, null, processingMethodsNumber++, "fake processing method - to be done", (int) srcSoftware.getId());
      processingMethods.add(pm);
    }

    List<Run> runs = srcReader.getRuns();
    List<Sample> samples = srcReader.getSamples();
    List<SourceFile> sourceFiles = srcReader.getSourceFiles();
    List<SharedParamTree> sharedParamTrees = srcReader.getSharedParamTreeList();
    List<CV> cvs = srcReader.getCvList();
    List<CVTerm> cvTerms = srcReader.getCvTermList();
    List<CVUnit> cvUnits = srcReader.getCvUnitList();

    LOG.info("Created MzDBMetaData.");
    MzDBMetaData mzMetaData = new MzDBMetaData();
    mzMetaData.setMzdbHeader(mzdbHeader);
    mzMetaData.setDataEncodings(dataEncodings);
    mzMetaData.setSharedParamTrees(sharedParamTrees);
    mzMetaData.setInstrumentConfigurations(instrumentConfigurations);
    mzMetaData.setProcessingMethods(processingMethods);
    mzMetaData.setRuns(runs);
    mzMetaData.setSamples(samples);
    mzMetaData.setSourceFiles(sourceFiles);
    mzMetaData.setSoftwares(softwares);
    mzMetaData.setCvList(cvs);
    mzMetaData.setCvTerms(cvTerms);
    mzMetaData.setCvUnits(cvUnits);

    return mzMetaData;
  }
}
