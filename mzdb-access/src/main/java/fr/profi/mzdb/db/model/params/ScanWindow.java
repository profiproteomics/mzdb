package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;

import javax.xml.bind.annotation.XmlRootElement;
import java.io.IOException;

/**
 * @author David Bouyssie
 *
 */
@XmlRootElement(name = "scanWindow")
public class ScanWindow extends AbstractParamTree {

    public ScanWindow() {

    }

    public ScanWindow(SerializationReader reader) throws IOException {
        read(reader);
    }
	
}