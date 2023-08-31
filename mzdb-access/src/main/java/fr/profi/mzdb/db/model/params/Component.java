package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAttribute;
import java.io.IOException;

// TODO: Auto-generated Javadoc
/**
 * The Class Component.
 * 
 * @author David Bouyssie
 */
public class Component extends AbstractParamTree {

    /** The order. */

    protected int order;

    public Component() {

    }

    public Component(SerializationReader reader) throws IOException {
        read(reader);
    }

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

    @Override
    public void write(SerializationWriter writer) throws IOException {
        super.write(writer);
        writer.writeInt32(order);

    }

    @Override
    public void read(SerializationReader reader) throws IOException {
        super.read(reader);
        order = reader.readInt32();
    }


}
