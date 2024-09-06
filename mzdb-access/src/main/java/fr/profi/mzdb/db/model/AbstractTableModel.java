package fr.profi.mzdb.db.model;

import java.io.IOException;
import java.util.List;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.db.model.params.IParamContainer;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.db.model.params.param.CVEntry;
import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.model.params.param.UserParam;
import fr.profi.mzdb.db.model.params.param.UserText;
import fr.profi.mzdb.io.reader.table.ParamTreeParser;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;
import fr.profi.mzdb.util.misc.AbstractInMemoryIdGen;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;

import fr.profi.mzdb.serialization.SerializationInterface;

// TODO: Auto-generated Javadoc
/**
 * The Class AbstractTableModel.
 * 
 * @author David Bouyssie
 */
public abstract class AbstractTableModel extends AbstractInMemoryIdGen implements IParamContainer, SerializationInterface {

	public static String TABLE_NAME;
	
	/** The id. */
	protected long id;

	/** The param tree. */
	protected ParamTree paramTree = null;


	protected AbstractTableModel() {
	}

	/**
	 * Instantiates a new abstract table model.
	 * 
	 * @param id
	 *            the id
	 * @param paramTree
	 *            the param tree
	 */
	protected AbstractTableModel(long id, ParamTree paramTree) {
		super();
		this.id = id;
		this.paramTree = paramTree;
	}

	/**
	 * Gets the id.
	 * 
	 * @return the id
	 */
	public long getId() {
		return id;
	}

	/*
	 * public void setId(int id) { this.id = id; }
	 */

	/**
	 * Checks for param tree.
	 * 
	 * @return true, if successful
	 */
	public boolean hasParamTree() {
		return paramTree != null;
	}

	/**
	 * Gets the param tree.
	 * 
	 * @return the param tree
	 */
	public ParamTree getParamTree(SQLiteConnection mzDbConnection) throws SQLiteException {
		if (!this.hasParamTree()) {
			this.loadParamTree(mzDbConnection);
		}
		return paramTree;
	}

	/**
	 * Sets the param tree.
	 * 
	 * @param paramTree
	 *            the new param tree
	 */
	public void setParamTree(ParamTree paramTree) {
		this.paramTree = paramTree;
	}
	
	/**
	 * Loads the param tree.
	 * 
	 *
	 */
	protected void loadParamTree(SQLiteConnection mzDbConnection) throws SQLiteException {
		this.paramTree = ParamTreeParser.parseParamTree(getParamTreeAsString(mzDbConnection));
	}

	public String getParamTreeAsString(SQLiteConnection mzDbConnection) throws SQLiteException {
		String sqlString = "SELECT param_tree FROM " + TABLE_NAME;
		return new SQLiteQuery(mzDbConnection, sqlString).extractSingleString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.db.model.IParamContainer#getCVParams()
	 */
	public List<CVParam> getCVParams() {
		return this.paramTree.getCVParams();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.db.model.IParamContainer#getUserParams()
	 */
	public List<UserParam> getUserParams() {
		return this.paramTree.getUserParams();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.db.model.IParamContainer#getUserParam(java.lang.String)
	 */
	public UserParam getUserParam(String name) {
		return this.paramTree.getUserParam(name);
	}

	public CVParam getCVParam(CVEntry cvEntry) {
		return this.paramTree.getCVParam(cvEntry);
	}

	public List<UserText> getUserTexts() {
		return this.paramTree.getUserTexts();
	}


	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeInt64(id);

		boolean hasParamTree = (paramTree != null);
		writer.writeBoolean(hasParamTree);
		if (hasParamTree) {
			paramTree.write(writer);
		}

	}

	@Override
	public void read(SerializationReader reader) throws IOException {

		id = reader.readInt64();

		boolean hasParamTree = reader.readBoolean();
		if (hasParamTree) {
			paramTree = new ParamTree(reader);
		} else {
			paramTree = null;
		}
	}

}