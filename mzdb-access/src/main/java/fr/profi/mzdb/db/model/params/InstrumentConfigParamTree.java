package fr.profi.mzdb.db.model.params;

import java.util.ArrayList;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;


// TODO: Auto-generated Javadoc
/**
 * Put one default constructor just in case.
 *
 * @author Marco
 */
@JacksonXmlRootElement(localName = "instrumentConfiguration")
public class InstrumentConfigParamTree {
	
	/** The id. */
	@JacksonXmlProperty(isAttribute = true, localName = "id")
	protected String id;
	
	//TODO see if the removing of count attribute of componentList tag could be avoided 
	/** The components. */
	@JacksonXmlProperty(isAttribute = true, localName = "componentList")
	protected ArrayList<Component> components;
	
	/**
	 * Instantiates a new instrument config param tree.
	 */
	public InstrumentConfigParamTree() {}
	
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
   * @param id the new id
   */
  public void setId(String id) {
    this.id = id;
  }
  
	/**
	 * Gets the components.
	 *
	 * @return the components
	 */
	public ArrayList<Component> getComponents() {
		return components;
	}

	/**
	 * Sets the components.
	 *
	 * @param components the new components
	 */
	public void setComponents(ArrayList<Component> components) {
		this.components = components;
	}

}
