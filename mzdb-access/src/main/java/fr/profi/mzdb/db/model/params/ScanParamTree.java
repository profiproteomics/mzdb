package fr.profi.mzdb.db.model.params;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementWrapper;

import fr.profi.mzdb.db.model.params.param.CVEntry;
import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.model.params.thermo.ThermoScanMetaData;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

public class ScanParamTree extends AbstractParamTree {	

	@XmlElementWrapper
	protected List<ScanWindowList> scanWindowList;

	public List<ScanWindowList> getScanWindowList() {
		return scanWindowList;
	}

	public ScanParamTree() {
	}

	public ScanParamTree(SerializationReader reader) throws IOException {
		read(reader);
	}
	
	public ThermoScanMetaData getThermoMetaData() {
		CVParam filterStringCvParam = this.getCVParam(CVEntry.FILTER_STRING);
		if (filterStringCvParam == null) return null;
		
		return new ThermoScanMetaData(filterStringCvParam.getValue());
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);

		writer.writeInt32(scanWindowList.size());
		for (SerializationInterface serializableObject : scanWindowList) {
			serializableObject.write(writer);
		}


	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		int size = reader.readInt32();
		scanWindowList = new ArrayList<>(size);
		for (int i = 0; i < size; i++) {
			ScanWindowList element = new ScanWindowList(reader);
			scanWindowList.add(element);
		}
	}
	
}
