package fr.profi.mzdb.db.model;

import java.time.Instant;

import fr.profi.mzdb.db.model.params.ParamTree;

// TODO: Auto-generated Javadoc
/**
 * The Class SourceFile.
 * 
 * @author David Bouyssie
 */
public class Run extends AbstractTableModel {
	
	public static final String TABLE_NAME = "run";

	/** The name. */
	protected String name;

	/** The location. */
	protected Instant startTimestamp;

	/**
	 * Instantiates a new source file.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param startTimestamp
	 *            the startTimestamp
	 * @param paramTree
	 *            the param tree
	 */
	public Run(int id, String name, Instant startTimestamp, ParamTree paramTree) {
		super(id, paramTree);
		this.name = name;
		this.startTimestamp = startTimestamp;
	}

	/**
	 * Instantiates a new source file.
	 * 
	 * @param id
	 *            the id
	 * @param name
	 *            the name
	 * @param startTimestamp
	 *            the startTimestamp
	 */
	public Run(int id, String name, Instant startTimestamp) {
		this(id, name, startTimestamp, null);
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
	 * @return the startTimestamp
	 */
	public Instant getStartTimestamp() {
		return startTimestamp;
	}

}
