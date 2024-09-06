package fr.profi.mzdb.db.model.params;


import fr.profi.mzdb.serialization.SerializationReader;

import javax.xml.bind.annotation.XmlRootElement;
import java.io.IOException;

@XmlRootElement(name = "fileContent")
public  class FileContentParams extends AbstractXMLParamTree {

	public FileContentParams() {
	}

	public FileContentParams(SerializationReader reader) throws IOException {
		read(reader);
	}

}
