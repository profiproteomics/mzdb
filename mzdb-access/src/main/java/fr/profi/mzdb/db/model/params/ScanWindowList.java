package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;

/**
 * @author David Bouyssie
 *
 */
@XmlRootElement(name = "scanWindowList")
public class ScanWindowList extends AbstractParamTree {
	
	@XmlAttribute(required = true)
	@XmlSchemaType(name = "nonNegativeInteger")
	protected int count;
	
	@XmlElementWrapper( name="scanWindow" )
	protected List<ScanWindow> scanWindows;
	
	public ScanWindowList() {
	}

	public ScanWindowList(SerializationReader reader) throws IOException {
		read(reader);
	}
	
	public ScanWindowList(int c) {
		this.count = c;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);

		writer.writeInt32(count);

		writer.writeInt32(scanWindows.size());
		for (SerializationInterface serializableObject : scanWindows) {
			serializableObject.write(writer);
		}


	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		count = reader.readInt32();

		int size = reader.readInt32();
		scanWindows = new ArrayList<>(size);
		for (int i=0;i<size;i++) {
			ScanWindow element = new ScanWindow(reader);
			scanWindows.add(element);
		}
	}

}