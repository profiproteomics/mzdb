package fr.profi.mzdb.io.reader;

import java.io.InputStream;
import java.io.StringReader;

import javax.xml.bind.JAXBException;
import javax.xml.transform.stream.StreamSource;

import org.xml.sax.InputSource;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.model.params.InstrumentConfigParamTree;
import fr.profi.mzdb.db.model.params.ParamTree;

// TODO: Auto-generated Javadoc
/**
 * The Class ParamTreeParser.
 * 
 * @author David Bouyssie
 */
public class ParamTreeParser {

	/**
	 * Parses the param tree.
	 * 
	 * @param paramTreeAsStr
	 *            the param tree as str
	 * @return the param tree
	 */
	public static ParamTree parseParamTree(String paramTreeAsStr) {

		ParamTree paramTree = null;
		try {
			paramTree = (ParamTree) MzDbReader.unmarshaller.unmarshal(new StringReader(paramTreeAsStr));
		} catch (JAXBException e) {
			e.printStackTrace();
		}
		return paramTree;
	}

	/**
	 * Parses the instrument config param tree.
	 * 
	 * @param paramTreeAsStr
	 *            the param tree as str
	 * @return the instrument config param tree
	 */
	public static InstrumentConfigParamTree parseInstrumentConfigParamTree(String paramTreeAsStr) {

		InstrumentConfigParamTree paramTree = null;
		try {
			paramTree = (InstrumentConfigParamTree)MzDbReader.instrumentConfigUnmarshaller.unmarshal(new InputSource(paramTreeAsStr));
		} catch (JAXBException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

		return paramTree;
	}

}
