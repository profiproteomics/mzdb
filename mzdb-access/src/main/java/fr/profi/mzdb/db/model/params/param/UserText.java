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


	public String getName() {
		return name;
	}

	public String getText() {
		return text;
	}

	public String getType() {
		return type;
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

		writer.writeString(name);

		boolean hasData = text!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(text);
		}

		writer.writeString(type);

	}

	@Override
	public void read(SerializationReader reader) throws IOException {

		name = reader.readString();

		boolean hasData = reader.readBoolean();
		if (hasData) {
			text = reader.readString();
		} else {
			text = null;
		}

		type = reader.readString();
	}

}
