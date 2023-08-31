package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

/**
 * @author Marco
 * 
 */
@XmlRootElement(name = "componentList")
public class ComponentList extends AbstractParamTree {

	protected List<Component> components;

	protected int count;

	public ComponentList(int c) {
		this.count = c;
	}

	public ComponentList() {
	}

	public ComponentList(SerializationReader reader) throws IOException {
		read(reader);
	}

	@XmlElements({
					@XmlElement(name = "detector", required = true, type = DetectorComponent.class),
					@XmlElement(name = "analyzer", required = true, type = AnalyzerComponent.class),
					@XmlElement(name = "source", required = true, type = SourceComponent.class)
	})

	public List<Component> getComponents() {
		return components;
	}

	@XmlAttribute(required = true)
	@XmlSchemaType(name = "nonNegativeInteger")
	public int getCount(){
		return  count;
	}

	public void setCount(int c){
		count = c;
	}

	public void setComponents(List<Component> components){
		this.components = components;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);

		writer.writeInt32(components.size());
		for (SerializationInterface serializableObject : components) {
			serializableObject.write(writer);
		}

		writer.writeInt32(count);

	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		int size = reader.readInt32();
		components = new ArrayList<>(size);
		for (int i=0;i<size;i++) {
			Component element = new Component(reader);
			components.add(element);
		}

		count = reader.readInt32();
	}

}
