package fr.profi.mzdb.db.model.params.param;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;

public class UserParam {

	@JacksonXmlProperty(isAttribute = true, localName = "name")
	protected String name;

	@JacksonXmlProperty(isAttribute = true, localName = "value")
	protected String value;

	@JacksonXmlProperty(isAttribute = true, localName = "type")
	protected String type;// ="xsd:float"/>;

	@JacksonXmlProperty(isAttribute = true, localName = "cvRef")
	protected String cvRef = "MS";

	@JacksonXmlProperty(isAttribute = true, localName = "accession")
	protected String accession = "";

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

}
