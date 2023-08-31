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
@XmlType(name="SourceComponent")
public class SourceComponent extends Component {

    public SourceComponent() {

    }

    public SourceComponent(SerializationReader reader) throws IOException {
        read(reader);
    }
}
