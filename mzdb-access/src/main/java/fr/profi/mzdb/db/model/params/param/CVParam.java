package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.*;
import java.io.IOException;

@XmlAccessorType(XmlAccessType.FIELD)
public class CVParam implements SerializationInterface {

	@XmlAttribute
	protected String cvRef;

	@XmlAttribute
	protected String accession;
	
	@XmlAttribute
	protected String name;
	
	@XmlAttribute
	protected String value;

	@XmlAttribute
	protected String unitCvRef;

	@XmlAttribute
	protected String unitAccession;

	@XmlAttribute
	protected String unitName;

	public CVParam() {
	}

	public CVParam(SerializationReader reader) throws IOException {
		read(reader);
	}

	public String getCvRef() {
		return cvRef;
	}
	
	public String getAccession() {
		return accession;
	}

	public String getName() {
		return name;
	}
	
	public String getValue() {
		return value;
	}

	public String getUnitCvRef() {
		return unitCvRef;
	}

	public String getUnitAccession() {
		return unitAccession;
	}

	public String getUnitName() {
		return unitName;
	}
	
	public void setCvRef(String cvRef) {
		this.cvRef = cvRef;
	}

	public void setAccession(String accession) {
		this.accession = accession;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public void setUnitCvRef(String unitCvRef) {
		this.unitCvRef = unitCvRef;
	}

	public void setUnitAccession(String unitAccession) {
		this.unitAccession = unitAccession;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeString(cvRef);
		writer.writeString(accession);
		writer.writeString(name);
		writer.writeString(value);
		writer.writeString(unitCvRef);
		writer.writeString(unitAccession);
		writer.writeString(unitName);


	}

	@Override
	public void read(SerializationReader reader) throws IOException  {
		cvRef = reader.readString();
		accession = reader.readString();
		name = reader.readString();
		value = reader.readString();
		unitCvRef = reader.readString();
		unitAccession = reader.readString();
		unitName = reader.readString();
	}


}
