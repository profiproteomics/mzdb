package fr.profi.mzdb.db.model.params;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import java.util.List;

/**
 * @author CB205360
 *
 */
@XmlRootElement(name = "precursorList")
public class PrecursorList extends AbstractParamTree {

	@XmlAttribute(required = true)
	@XmlSchemaType(name = "nonNegativeInteger")
	protected int count;

	@XmlElement( name="precursor" )
	protected List<Precursor> precursors;

	public PrecursorList() {
	}

	public PrecursorList(int c) {
		this.count = c;
	}

	public List<Precursor> getPrecursors() {
		return precursors;
	}

}