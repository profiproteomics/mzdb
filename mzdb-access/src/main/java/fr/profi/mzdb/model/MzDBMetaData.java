package fr.profi.mzdb.model;

import fr.profi.mzdb.db.model.*;
import fr.profi.mzdb.db.model.params.param.CV;
import fr.profi.mzdb.db.model.params.param.CVTerm;
import fr.profi.mzdb.db.model.params.param.CVUnit;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class MzDBMetaData implements SerializationInterface {

  protected MzDbHeader mzdbHeader;
  protected List<DataEncoding> dataEncodings = new ArrayList<>();
  protected List<InstrumentConfiguration> instrumentConfigurations= new ArrayList<>();
  protected List<SharedParamTree> sharedParamTrees = new ArrayList<>();
  protected List<ProcessingMethod> processingMethods = new ArrayList<>();
  protected List<Run> runs= new ArrayList<>();
  protected List<Sample> samples= new ArrayList<>();
  protected List<Software> softwares= new ArrayList<>();
  protected List<SourceFile> sourceFiles= new ArrayList<>();
  protected List<CV> cvList = new ArrayList<>();
  protected List<CVTerm> cvTerms = new ArrayList<>();
  protected List<CVUnit> cvUnits = new ArrayList<>();

  protected double lowestMS1Mz = 0.0;

  public MzDBMetaData(){
    //Init list as empty
  }

  public MzDBMetaData(SerializationReader reader) throws IOException {
    read(reader);
  }

  public MzDbHeader getMzdbHeader() {
    return mzdbHeader;
  }

  public void setMzdbHeader(MzDbHeader mzdbHeader) {
    this.mzdbHeader = mzdbHeader;
  }

  public List<DataEncoding> getDataEncodings() {
    return dataEncodings;
  }

  public void setDataEncodings(List<DataEncoding> dataEncodings) {
    this.dataEncodings = dataEncodings;
  }

  public List<InstrumentConfiguration> getInstrumentConfigurations() {
    return instrumentConfigurations;
  }

  public void setInstrumentConfigurations(List<InstrumentConfiguration> instrumentConfigurations) {
    this.instrumentConfigurations = instrumentConfigurations;
  }

  public List<SharedParamTree>  getSharedParamTrees() {
    return sharedParamTrees;
  }

  public void setSharedParamTrees(List<SharedParamTree> sharedParamTrees) {
    this.sharedParamTrees = sharedParamTrees;
  }

  public List<ProcessingMethod> getProcessingMethods() {
    return processingMethods;
  }

  public void setProcessingMethods(List<ProcessingMethod> processingMethods) {
    this.processingMethods = processingMethods;
  }

  public List<Run> getRuns() {
    return runs;
  }

  public void setRuns(List<Run> runs) {
    this.runs = runs;
  }

  public List<Sample> getSamples() {
    return samples;
  }

  public void setSamples(List<Sample> samples) {
    this.samples = samples;
  }

  public List<Software> getSoftwares() {
    return softwares;
  }

  public void setSoftwares(List<Software> softwares) {
    this.softwares = softwares;
  }

  public List<SourceFile> getSourceFiles() {
    return sourceFiles;
  }

  public void setSourceFiles(List<SourceFile> sourceFiles) {
    this.sourceFiles = sourceFiles;
  }

  public List<CV> getCvList() {
    return cvList;
  }

  public void setCvList(List<CV> cvList) {
    this.cvList = cvList;
  }

  public List<CVTerm> getCvTerms() {
    return cvTerms;
  }

  public void setCvTerms(List<CVTerm> cvTerms) {
    this.cvTerms = cvTerms;
  }

  public List<CVUnit> getCvUnits() {
    return cvUnits;
  }

  public void setCvUnits(List<CVUnit> cvUnits) {
    this.cvUnits = cvUnits;
  }

  public double getLowestMS1Mz() {
    return lowestMS1Mz;
  }

  public void setLowestMS1Mz(double lowestMS1Mz) {
    this.lowestMS1Mz = lowestMS1Mz;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {

    mzdbHeader.write(writer);

    writeList(writer,  dataEncodings);
    writeList(writer, instrumentConfigurations);
    writeList(writer, sharedParamTrees);
    writeList(writer, processingMethods);
    writeList(writer, runs);
    writeList(writer, samples);
    writeList(writer, softwares);
    writeList(writer, sourceFiles);
    writeList(writer, cvList);
    writeList(writer, cvTerms);
    writeList(writer, cvUnits);

    writer.writeDouble(lowestMS1Mz); //JPM.OM.FIX

  }

  private void writeList(SerializationWriter writer, List list) throws IOException {
    writer.writeInt32(list.size());
    for (Object serializableObject : list) {
      ((SerializationInterface) serializableObject).write(writer);
    }
  }


  @Override
  public void read(SerializationReader reader) throws IOException {

    mzdbHeader = new MzDbHeader(reader);

    int size = reader.readInt32();
    dataEncodings = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      DataEncoding element = new DataEncoding(reader);
      dataEncodings.add(element);
    }

    size = reader.readInt32();
    instrumentConfigurations = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      InstrumentConfiguration element = new InstrumentConfiguration(reader);
      instrumentConfigurations.add(element);
    }

    size = reader.readInt32();
    sharedParamTrees = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      SharedParamTree element = new SharedParamTree(reader);
      sharedParamTrees.add(element);
    }

    size = reader.readInt32();
    processingMethods = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      ProcessingMethod element = new ProcessingMethod(reader);
      processingMethods.add(element);
    }

    size = reader.readInt32();
    runs = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      Run element = new Run(reader);
      runs.add(element);
    }

    size = reader.readInt32();
    samples = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      Sample element = new Sample(reader);
      samples.add(element);
    }

    size = reader.readInt32();
    softwares = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      Software element = new Software(reader);
      softwares.add(element);
    }

    size = reader.readInt32();
    sourceFiles = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      SourceFile element = new SourceFile(reader);
      sourceFiles.add(element);
    }

    size = reader.readInt32();
    cvList = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      CV element = new CV(reader);
      cvList.add(element);
    }

    size = reader.readInt32();
    cvTerms = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      CVTerm element = new CVTerm(reader);
      cvTerms.add(element);
    }

    size = reader.readInt32();
    cvUnits = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      CVUnit element = new CVUnit(reader);
      cvUnits.add(element);
    }

    lowestMS1Mz = reader.readDouble();

  }


}
