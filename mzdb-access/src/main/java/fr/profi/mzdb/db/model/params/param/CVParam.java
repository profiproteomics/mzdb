package fr.profi.mzdb.db.model.params.param;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

@JacksonXmlRootElement(localName = "cvParam")
public class CVParam {
	@JacksonXmlProperty(isAttribute = true, localName = "value")
	protected String value ="";
	@JacksonXmlProperty(isAttribute = true, localName = "cvRef")
	protected String cvRef = "MS";
	@JacksonXmlProperty(isAttribute = true, localName = "name")
	protected String name="";
	@JacksonXmlProperty(isAttribute = true, localName = "accession")
	protected String accession="";

	@JacksonXmlProperty(isAttribute = true, localName = "unitCvRef")
	protected String unitCvRef = "UO";
	@JacksonXmlProperty(isAttribute = true, localName = "unitAccession")
	protected String unitAccession = "";
	@JacksonXmlProperty(isAttribute = true, localName = "unitName")
	protected String unitName = "";

	public String getValue() {
		return value;
	}

	public String getAccession() {
		return accession;
	}

	public String getCvRef() {
		return cvRef;
	}

	public String getName() {
		return name;
	}

	public String getUnitCvRef() {
		return unitCvRef;
	}

	public String getUnitAccession() {
		return unitAccession;
	}

	public String getUnitName() {
		return unitName;
	}

}
