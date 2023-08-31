package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;

import javax.xml.bind.annotation.XmlRootElement;
import java.io.IOException;

@XmlRootElement(name="isolationWindow")
public class IsolationWindowParamTree extends AbstractParamTree {
    public IsolationWindowParamTree() {
	
    }

    public IsolationWindowParamTree(SerializationReader reader) throws IOException {
        read(reader);
    }
}
