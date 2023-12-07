package fr.profi.mzdb.db.model;


import fr.profi.mzdb.db.model.params.FileContentParams;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

/**
 * The Class MzDbHeader.
 * 
 * @author David Bouyssie
 */
public class MzDbHeader extends AbstractTableModel implements SerializationInterface {

	public static final String TABLE_NAME = "mzdb";

	/**
	 * The version.
	 */
	protected String version;

	/**
	 * The creation timestamp.
	 */
	protected int creationTimestamp;

	protected FileContentParams fileContent;


	public MzDbHeader(SerializationReader reader) throws IOException {
		read(reader);
	}

	/**
	 * Instantiates a new mz db header.
	 *
	 * @param version           the version
	 * @param creationTimestamp the creation timestamp
	 * @param paramTree         the param tree
	 */
	public MzDbHeader(String version, int creationTimestamp, ParamTree paramTree, FileContentParams fileContent) {
		super(1, paramTree);
		this.version = version;
		this.creationTimestamp = creationTimestamp;
		this.fileContent = fileContent;
	}

	/**
	 * Instantiates a new mz db header.
	 *
	 * @param version           the version
	 * @param creationTimestamp the creation timestamp
	 */
	public MzDbHeader(String version, int creationTimestamp) {
		this(version, creationTimestamp, null, null);
	}

	public String getVersion() {
		return this.version;
	}

	public FileContentParams getFileContent(){
		return fileContent;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		super.write(writer);

		writer.writeString(version);
		writer.writeInt32(creationTimestamp);

		//VDS SQL Not Null
		boolean hasData = fileContent!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			fileContent.write(writer);
		}

	}

	@Override
	public void read(SerializationReader reader) throws IOException {

		super.read(reader);

		version = reader.readString();
		creationTimestamp = reader.readInt32();

		boolean hasData = reader.readBoolean();
		if (hasData) {
			fileContent = new FileContentParams(reader);
		} else {
			fileContent = null;
		}
	}


}


