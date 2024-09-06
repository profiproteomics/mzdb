package fr.profi.mzdb.db.model;

import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

// TODO: Auto-generated Javadoc
/**
 * The Class Software.
 * 
 * @author David Bouyssie
 */
public class Software extends AbstractTableModel {
	
	public static final String TABLE_NAME = "software";

	/** The name. */
	protected String name;

	/** The version. */
	protected String version;

	public Software(SerializationReader reader) throws IOException {
		read(reader);
	}

	/**
	 * Instantiates a new software.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param version
	 *            the version
	 * @param paramTree
	 *            the param tree
	 */
	public Software(int id, String name, String version, ParamTree paramTree) {
		super(id, paramTree);
		this.name = name;
		this.version = version;
	}

	/**
	 * Instantiates a new software.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param version
	 *            the version
	 */
	public Software(int id, String name, String version) {
		this(id, name, version, null);
	}

	/**
	 * Gets the name.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/*
	 * public void setName(String name) { this.name = name; }
	 */

	/**
	 * Gets the version.
	 * 
	 * @return the version
	 */
	public String getVersion() {
		return version;
	}

	/*
	 * public void setVersion(String version) { this.version = version; }
	 */

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);

		writer.writeString(name);
		writer.writeString(version);

	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		name = reader.readString();
		version = reader.readString();
	}
}
