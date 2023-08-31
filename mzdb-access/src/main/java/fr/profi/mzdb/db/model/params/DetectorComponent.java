/**
 * 
 */
package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;

import javax.xml.bind.annotation.XmlType;
import java.io.IOException;

/**
 * @author Marco
 *
 */
@XmlType(name="DetectorComponent")
public class DetectorComponent extends Component {

    public DetectorComponent() {
    }

    public DetectorComponent(SerializationReader reader) throws IOException {
        read(reader);
    }
}
