package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;
import java.io.IOException;

/**
 * @author Marco
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class UserText implements SerializationInterface {
	
	@XmlAttribute
	protected String cvRef;
	
	@XmlAttribute
	protected String accession;
	
	@XmlAttribute
	protected String name;
	
	@XmlValue
	protected String text;
	
	@XmlAttribute
	protected String type;

	public UserText() {
	}

	public UserText(SerializationReader reader) throws IOException {
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

	public String getText() {
		return text;
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

	public void setText(String text) {
		this.text = text;
	}

	public void setType(String type) {
		this.type = type;
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

		hasData = text!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(text);
		}

		hasData = type!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(type);
		}


	}

	@Override
	public void read(SerializationReader reader) throws IOException {

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
			text = reader.readString();
		} else {
			text = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			type = reader.readString();
		} else {
			type = null;
		}
	}

}
