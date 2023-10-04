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
	protected String name;

	@XmlAttribute
	protected String value;

	@XmlAttribute
	protected String type;// ="xsd:float"/>;

	@XmlAttribute
	protected String unitAccession;
	
	public  UserParam() {
	}

	public UserParam(SerializationReader reader) throws IOException {
		read(reader);
	}

	public UserParam(  String name, String value, String type, String unitAccession){
		this.name = name;
		this.value = value;
		this.type = type;
		this.unitAccession = unitAccession;
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

	public String getUnitAccession() {
		return unitAccession;
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

	public void setUnitAccession(String unitAccession) {
		this.unitAccession = unitAccession;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeString(name);

		boolean hasData = value!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(value);
		}

		writer.writeString(type);

		hasData = unitAccession!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(unitAccession);
		}

	}

	@Override
	public void read(SerializationReader reader) throws IOException  {

		name = reader.readString();

		boolean hasData = reader.readBoolean();
		if (hasData) {
			value = reader.readString();
		} else {
			value = null;
		}

		type = reader.readString();
		hasData = reader.readBoolean();
		if (hasData) {
			unitAccession = reader.readString();
		} else {
			unitAccession = null;
		}
	}
}
