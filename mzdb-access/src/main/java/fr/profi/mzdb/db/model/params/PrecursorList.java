package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author CB205360
 *
 */
@XmlRootElement(name = "precursorList")
public class PrecursorList extends AbstractParamTree {

	@XmlAttribute(required = true)
	@XmlSchemaType(name = "nonNegativeInteger")
	protected int count;

	@XmlElement( name="precursor" )
	protected List<Precursor> precursors;

	public PrecursorList() {
	}

	public PrecursorList(SerializationReader reader) throws IOException {
		read(reader);
	}

	public PrecursorList(int c) {
		this.count = c;
	}

	public List<Precursor> getPrecursors() {
		return precursors;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);


		writer.writeInt32(count);

		writer.writeInt32(precursors.size());
		for (SerializationInterface serializableObject : precursors) {
			serializableObject.write(writer);
		}


	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		count = reader.readInt32();

		int size = reader.readInt32();
		precursors = new ArrayList<>(size);
		for (int i=0;i<size;i++) {
			Precursor element = new Precursor(reader);
			precursors.add(element);
		}

	}

}