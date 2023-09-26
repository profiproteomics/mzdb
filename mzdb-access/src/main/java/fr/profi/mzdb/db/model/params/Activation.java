package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;

import javax.xml.bind.annotation.XmlRootElement;
import java.io.IOException;

@XmlRootElement(name="activation")
public class Activation extends AbstractXMLParamTree{
    public Activation() {}

    public Activation(SerializationReader reader) throws IOException {
        read(reader);
    }
}
