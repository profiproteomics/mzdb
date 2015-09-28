package fr.profi.mzdb.io.reader.cache;

import fr.profi.mzdb.AbstractMzDbReader;

/**
 * @author David Bouyssie
 *
 */
public abstract class MzDbEntityCacheContainer {

	/** The mzDB reader. */
	protected AbstractMzDbReader mzDbReader = null;
	protected MzDbEntityCache entityCache = null;

	/**
	 * Instantiates a new abstract mz db reader helper.
	 *
	 * @param mzDbReader
	 *            the mz db reader
	 */
	public MzDbEntityCacheContainer(AbstractMzDbReader mzDbReader) {
		super();
		this.mzDbReader = mzDbReader;
		this.entityCache = mzDbReader.getEntityCache();
	}

}
