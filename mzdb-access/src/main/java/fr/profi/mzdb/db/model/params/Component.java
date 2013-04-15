package fr.profi.mzdb.db.model.params;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;

// TODO: Auto-generated Javadoc
/**
 * The Class Component.
 * 
 * @author David Bouyssie
 */
public class Component extends ParamTree {

	/** The order. */
	@JacksonXmlProperty(isAttribute = true, localName = "order")
	protected int order;

	/** The type. */
	protected ComponentType type = null;

	/**
	 * Instantiates a new component.
	 * 
	 * @param order
	 *            the order
	 */
	public Component(int order) {
		super();
		this.order = order;
	}

	/**
	 * Gets the type.
	 * 
	 * @return the type
	 */
	public ComponentType getType() {
		if (type == null) {
			int o = this.order;
			if (o == 1) {
				type = ComponentType.SOURCE;
			} else if (o == 2) {
				type = ComponentType.ANALYZER;
			} else if (o == 3) {
				type = ComponentType.DETECTOR;
			} else {
				System.out.println("Error : " + order);
			}
		}
		return type;
	}

	/**
	 * Gets the order.
	 * 
	 * @return the order
	 */
	public int getOrder() {
		return order;
	}

}
