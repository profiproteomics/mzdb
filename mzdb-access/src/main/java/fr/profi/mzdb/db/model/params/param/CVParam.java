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

		boolean hasData = cvRef!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(cvRef);
		}

		hasData = accession!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(accession);
		}

		hasData = name!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(name);
		}

		hasData = value!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(value);
		}

		hasData = unitCvRef!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(unitCvRef);
		}

		hasData = unitAccession!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(unitAccession);
		}

		hasData = unitName!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(unitName);
		}


	}

	@Override
	public void read(SerializationReader reader) throws IOException  {

		boolean hasData = reader.readBoolean();
		if (hasData) {
			cvRef = reader.readString();
		} else {
			cvRef = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			accession = reader.readString();
		} else {
			accession = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			name = reader.readString();
		} else {
			name = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			value = reader.readString();
		} else {
			value = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			unitCvRef = reader.readString();
		} else {
			unitCvRef = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			unitAccession = reader.readString();
		} else {
			unitAccession = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			unitName = reader.readString();
		} else {
			unitName = null;
		}
	}


}
