package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAttribute;
import java.io.IOException;
import java.util.HashMap;

// TODO: Auto-generated Javadoc
/**
 * The Class Component.
 * 
 * @author David Bouyssie
 */
public abstract class Component extends AbstractParamTree {

    /** The order. */

    protected int order;

    //protected ComponentType type;

    public Component() {
    }

    public Component(SerializationReader reader) throws IOException {
        read(reader);
    }

    public abstract ComponentType getType();

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

    public enum ComponentType {
        DETECTOR(0), ANALYZER(1), SOURCE(2);

        private final int type;

        private static HashMap<Integer, ComponentType> map = new HashMap<>();

        static {
            for (ComponentType aType : ComponentType.values()) {
                map.put(aType.type, aType);
            }
        }

        private ComponentType(int type) {
            this.type = type;
        }

        public int getTypeValue() {
            return type;
        }

        public static ComponentType getEnum(int value) throws IOException {
            return map.get(value);
        }
    }

}
