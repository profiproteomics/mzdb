package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import java.io.IOException;

@XmlAccessorType(XmlAccessType.FIELD)
public class UserParam implements SerializationInterface {
	
	@XmlAttribute
	protected String cvRef;

	@XmlAttribute
	protected String accession;

	@XmlAttribute
	protected String name;

	@XmlAttribute
	protected String value;

	@XmlAttribute
	protected String type;// ="xsd:float"/>;

	public  UserParam() {
	}

	public UserParam(SerializationReader reader) throws IOException {
		read(reader);
	}

	public UserParam( String cvRef, String accession, String name, String value, String type){
		this.cvRef = cvRef;
		this.accession = accession;
		this.name = name;
		this.value = value;
		this. type = type;
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

	public String getType() {
		return type;
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

	public void setType(String type) {
		this.type = type;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeString(cvRef);
		writer.writeString(accession);
		writer.writeString(name);
		writer.writeString(value);
		writer.writeString(type);
	}

	@Override
	public void read(SerializationReader reader) throws IOException  {

		cvRef = reader.readString();
		accession = reader.readString();
		name = reader.readString();
		value = reader.readString();
		type = reader.readString();
	}
}
