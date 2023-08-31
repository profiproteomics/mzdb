package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;

/**
 * @author David Bouyssie
 * 
 */
@XmlRootElement(name = "scanList")
public class ScanList extends AbstractParamTree {

	@XmlAttribute(required = true)
	@XmlSchemaType(name = "nonNegativeInteger")
	protected int count;

	@XmlElement( name="scan" )
	protected List<ScanParamTree> scans;

	public ScanList() {
	}

	public ScanList(SerializationReader reader) throws IOException {
		read(reader);
	}
	
	public ScanList(int c) {
		this.count = c;
	}
	
	public List<ScanParamTree> getScans() {
		return scans;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);

		writer.writeInt32(count);

		writer.writeInt32(scans.size());
		for (SerializationInterface serializableObject : scans) {
			serializableObject.write(writer);
		}


	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		count = reader.readInt32();

		int size = reader.readInt32();
		scans = new ArrayList<>(size);
		for (int i=0;i<size;i++) {
			ScanParamTree element = new ScanParamTree(reader);
			scans.add(element);
		}
	}

}
