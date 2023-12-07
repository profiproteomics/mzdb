package fr.profi.mzdb.db.model;

import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

// TODO: Auto-generated Javadoc
/**
 * The Class SourceFile.
 * 
 * @author David Bouyssie
 */
public class SourceFile extends AbstractTableModel {
	
	public static final String TABLE_NAME = "source_file";

	/** The name. */
	protected String name;

	/** The location. */
	protected String location;

	public SourceFile(SerializationReader reader) throws IOException {
		read(reader);
	}

	/**
	 * Instantiates a new source file.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param location
	 *            the location
	 * @param paramTree
	 *            the param tree
	 */
	public SourceFile(int id, String name, String location, ParamTree paramTree) {
		super(id, paramTree);
		this.name = name;
		this.location = location;
	}

	/**
	 * Instantiates a new source file.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param location
	 *            the location
	 */
	public SourceFile(int id, String name, String location) {
		this(id, name, location, null);
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
	 * Gets the location.
	 * 
	 * @return the location
	 */
	public String getLocation() {
		return location;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		super.write(writer);

		writer.writeString(name);
		writer.writeString(location);

	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		super.read(reader);

		name = reader.readString();
		location = reader.readString();
	}
}
