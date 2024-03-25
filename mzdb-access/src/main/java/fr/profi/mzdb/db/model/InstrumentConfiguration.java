package fr.profi.mzdb.db.model;

import fr.profi.mzdb.db.model.params.ComponentList;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;
import fr.profi.mzdb.util.misc.AbstractInMemoryIdGen;

import java.io.IOException;

// TODO: Auto-generated Javadoc
/**
 * The Class InstrumentConfiguration.
 * 
 * @author David Bouyssie
 */
public class InstrumentConfiguration extends AbstractInMemoryIdGen implements SerializationInterface {
	
	public static final String TABLE_NAME = "instrument_configuration";

	/** The id. */
	protected long id;

	/** The name. */
	protected String name;

	/** The software id. */
	protected int softwareId;

	
	protected ParamTree paramTree;
	
	/** The param tree. */
	protected ComponentList componentList;

	public InstrumentConfiguration(SerializationReader reader) throws IOException {
		read(reader);
	}

	/**
	 * Instantiates a new instrument configuration.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param softwareId
	 *            the software id
	 * @param paramTree
	 *            the param tree
	 */
	public InstrumentConfiguration(long id, String name, int softwareId, ParamTree paramTree, ComponentList comp) {
		super();
		this.id = id;
		this.name = name;
		this.softwareId = softwareId;
		this.paramTree = paramTree;
		this.componentList = comp;
	}

	/**
	 * Instantiates a new instrument configuration.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param softwareId
	 *            the software id
	 */
	public InstrumentConfiguration(long id, String name, int softwareId) {
		this(id, name, softwareId, null, null);
	}

	/**
	 * Gets the id.
	 * 
	 * @return the id
	 */
	public long getId() {
		return id;
	}

	/**
	 * Gets the name.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the software id.
	 * 
	 * @return the software id
	 */
	public int getSoftwareId() {
		return softwareId;
	}

	public ParamTree getParamTree() {
	  return this.paramTree;
	}
	
	/**
	 * Gets the param tree.
	 * 
	 * @return the param tree
	 */
	public ComponentList getComponentList() {
		return componentList;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeInt64(id);
		writer.writeString(name);
		writer.writeInt32(softwareId);

		boolean hasData = paramTree!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			paramTree.write(writer);
		}

		//VDS SQL Not Null
		hasData = componentList!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			componentList.write(writer);
		}

	}

	@Override
	public void read(SerializationReader reader) throws IOException {

		id = reader.readInt64();
		name = reader.readString();
		softwareId = reader.readInt32();

		boolean hasData = reader.readBoolean();
		if (hasData) {
			paramTree = new ParamTree(reader);
		} else {
			paramTree = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			componentList = new ComponentList(reader);
		} else {
			componentList = null;
		}

	}

	/**
	 * Sets the param tree.
	 * 
	 * @param paramTree
	 *            the new param tree
	 */
	/*public void setParamTree(InstrumentConfigParamTree paramTree) {
		this.paramTree = paramTree;
	}*/

}
