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
@XmlType(name="AnalyzerComponent")
public class AnalyzerComponent extends Component {

    public AnalyzerComponent() {

    }

    public AnalyzerComponent(SerializationReader reader) throws IOException {
        read(reader);
    }
}
