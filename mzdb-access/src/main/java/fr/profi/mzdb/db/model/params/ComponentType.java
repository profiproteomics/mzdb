package fr.profi.mzdb.db.model.params;

// TODO: Auto-generated Javadoc
/**
 * The Enum ComponentType.
 *
 * @author David Bouyssie
 */
public enum ComponentType {
	
	/** The source. */
	SOURCE(1),
	
	/** The analyzer. */
	ANALYZER(2),
	
	/** The detector. */
	DETECTOR(3);
	
	/** The type. */
	private final int type;
	
	/**
	 * Instantiates a new component type.
	 *
	 * @param type_ the type_
	 */
	private ComponentType(int type_) {
		this.type = type_;
	}
	
	/**
	 * Gets the type.
	 *
	 * @return the type
	 */
	public int getType() {
		return type;
	}
}
