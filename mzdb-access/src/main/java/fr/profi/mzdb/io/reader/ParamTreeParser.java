package fr.profi.mzdb.io.reader;

import java.io.StringReader;

import javax.xml.bind.JAXBException;

import org.xml.sax.InputSource;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.model.params.ComponentList;
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
	synchronized public static ParamTree parseParamTree(String paramTreeAsStr) {

		ParamTree paramTree = null;
		try {
			paramTree = (ParamTree) MzDbReader.unmarshaller.unmarshal(new StringReader(paramTreeAsStr));
		} catch (JAXBException e) {
			e.printStackTrace();
		}
		ParamTreeParser.class.notifyAll();
		return paramTree;
	}

	/**
	 * Parses the instrument config param tree.
	 * 
	 * @param paramTreeAsStr
	 *            the param tree as str
	 * @return the instrument config param tree
	 */
	synchronized public static ComponentList parseComponentList(String paramTreeAsStr) {

		ComponentList paramTree = null;
		try {
			paramTree = (ComponentList)MzDbReader.instrumentConfigUnmarshaller.unmarshal(new InputSource(paramTreeAsStr));
		} catch (JAXBException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
		ParamTreeParser.class.notifyAll();
		return paramTree;
	}

}
