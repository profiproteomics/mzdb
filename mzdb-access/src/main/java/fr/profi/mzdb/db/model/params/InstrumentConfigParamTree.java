package fr.profi.mzdb.db.model.params;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

// TODO: Auto-generated Javadoc
/**
 * Put one default constructor just in case.
 * 
 * @author Marco
 */
@XmlRootElement(name = "instrumentConfiguration")
public class InstrumentConfigParamTree {

	/** The id. */
  @XmlAttribute(required=true)
	protected String id;

	// TODO see if the removing of count attribute of componentList tag could be
	// avoided
	/** The components. */
	
  @XmlElements({
    @XmlElement(name = "detector", required = true, type = DetectorComponent.class),
    @XmlElement(name = "analyzer", required = true, type = AnalyzerComponent.class),
    @XmlElement(name = "source", required = true, type = SourceComponent.class)
  })
  @XmlElementWrapper
	protected List<Component> components;

	/**
	 * Instantiates a new instrument config param tree.
	 */
	public InstrumentConfigParamTree() {
	}

	/**
	 * Gets the id.
	 * 
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the id.
	 * 
	 * @param id
	 *            the new id
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * Gets the components.
	 * 
	 * @return the components
	 */
	public List<Component> getComponents() {
		return components;
	}

	/**
	 * Sets the components.
	 * 
	 * @param components
	 *            the new components
	 */
	public void setComponents(ArrayList<Component> components) {
		this.components = components;
	}

}
