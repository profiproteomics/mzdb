package fr.profi.mzdb.db.model.params;

import javax.xml.bind.annotation.XmlAttribute;

// TODO: Auto-generated Javadoc
/**
 * The Class Component.
 * 
 * @author David Bouyssie
 */
public class Component extends AbstractParamTree {

    /** The order. */

    protected int order;

    /**
     * Gets the order.
     * 
     * @return the order
     */
    @XmlAttribute
    public int getOrder() {
	      return order;
    }

    public void setOrder(int order){
        this.order = order;
    }
}
