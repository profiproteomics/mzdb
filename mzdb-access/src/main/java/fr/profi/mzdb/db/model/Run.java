package fr.profi.mzdb.db.model;

import java.util.Date;

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
	protected Date startTimestamp;

	//Other properties which may be null
	protected Integer sampleId;
	protected Integer instrumentConfigId;
	protected Integer sourceFileId;

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
	public Run(int id, String name, Date startTimestamp, ParamTree paramTree) {
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
	public Run(int id, String name, Date startTimestamp) {
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
	public Date getStartTimestamp() {
		return startTimestamp;
	}

	public Integer getSampleId() {
		return sampleId;
	}

	public void setSampleId(Integer sampleId) {
		this.sampleId = sampleId;
	}

	public Integer getInstrumentConfigId() {
		return instrumentConfigId;
	}

	public void setInstrumentConfigId(Integer instrumentConfigId) {
		this.instrumentConfigId = instrumentConfigId;
	}

	public Integer getSourceFileId() {
		return sourceFileId;
	}

	public void setSourceFileId(Integer sourceFileId) {
		this.sourceFileId = sourceFileId;
	}
}
