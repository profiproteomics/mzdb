package fr.profi.mzdb.db.model.params;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.model.params.param.UserParam;

// TODO: Auto-generated Javadoc
/**
 * The Class ParamTree.
 * 
 * @author David Bouyssie
 */
@JacksonXmlRootElement(localName = "params")
public class ParamTree implements IParamContainer {

	/** The cv params. */
	@JacksonXmlProperty(isAttribute = true, localName = "cvParams")
	protected List<CVParam> cvParams;

	/** The user params. */
	@JacksonXmlProperty(isAttribute = true, localName = "userParams")
	protected List<UserParam> userParams;

	/**
	 * necessary for jackson.
	 */
	protected ParamTree() {
		super();
	}

	/*
	 * public ParamTree( IParamContainer params ) { this(); if( params != null ) { this.cvParams =
	 * params.getCVParams(); this.userParams = params.getUserParams(); } }
	 */

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.db.model.IParamContainer#getCVParams()
	 */
	public List<CVParam> getCVParams() {
		if (this.cvParams == null)
			this.cvParams = new ArrayList<CVParam>();

		return cvParams;
	}

	// most of the object does not contain any UserParam, so this is set to be non
	// abstract
	// to avoid to override it in subclasses
	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.db.model.IParamContainer#getUserParams()
	 */
	public List<UserParam> getUserParams() {
		if (this.userParams == null)
			this.userParams = new ArrayList<UserParam>();

		return this.userParams;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.db.model.IParamContainer#getUserParam(java.lang.String)
	 */
	public UserParam getUserParam(String name) {

		UserParam p = null;
		for (UserParam up : this.getUserParams()) {
			if (up.getName().equals(name)) {
				p = up;
				break;
			}
		}
		return p;
	}
}
