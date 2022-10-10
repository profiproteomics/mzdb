package fr.profi.mzdb.io.reader.table;

import fr.profi.mzdb.db.model.params.*;
import fr.profi.mzdb.util.jaxb.XercesSAXParser;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.sax.SAXSource;

// TODO: Auto-generated Javadoc
/**
 * The Class ParamTreeParser.
 * 
 * @author David Bouyssie
 */
public class ParamTreeParser {
	
	/** The xml mappers. */
	public static Unmarshaller paramTreeUnmarshaller = null;
	public static Unmarshaller fileContentUnmarshaller = null;
	public static Unmarshaller componentListUnmarshaller = null;
	public static Unmarshaller scanListUnmarshaller = null;
	public static Unmarshaller precursorUnmarshaller = null;
	public static Unmarshaller precursorListUnmarshaller = null;
	public static Unmarshaller refParamGroupUnmarshaller = null;

	/**
	 * Parses the param tree.
	 * 
	 * @param paramTreeAsStr The param tree as a String
	 * @return the param tree
	 */
	synchronized public static ParamTree parseParamTree(String paramTreeAsStr) {
		
		ParamTree paramTree = null;
		if(paramTreeAsStr == null || paramTreeAsStr.isBlank())
			return null;
		try {
			if( paramTreeUnmarshaller == null ) {
				paramTreeUnmarshaller = JAXBContext.newInstance(ParamTree.class).createUnmarshaller();
			}
			
			SAXSource source = XercesSAXParser.getSAXSource( paramTreeAsStr );
			paramTree = (ParamTree) paramTreeUnmarshaller.unmarshal(source);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return paramTree;
	}

	synchronized public static FileContentParams parseFileContent(String paramTreeAsStr) {

		FileContentParams paramTree = null;
		if(paramTreeAsStr == null || paramTreeAsStr.isBlank())
			return null;
		try {
			if( fileContentUnmarshaller == null ) {
				fileContentUnmarshaller = JAXBContext.newInstance(FileContentParams.class).createUnmarshaller();
			}

			SAXSource source = XercesSAXParser.getSAXSource( paramTreeAsStr );
			paramTree = (FileContentParams) fileContentUnmarshaller.unmarshal(source);

		} catch (Exception e) {
			e.printStackTrace();
		}

		return paramTree;
	}

	synchronized public static ScanList parseScanList(String scanListAsStr) {

		ScanList scanList = null;
		if(scanListAsStr == null || scanListAsStr.isBlank())
			return null;
		try {
			if( scanListUnmarshaller == null ) {
				scanListUnmarshaller = JAXBContext.newInstance(ScanList.class).createUnmarshaller();
			}
			
			SAXSource source = XercesSAXParser.getSAXSource( scanListAsStr );
			scanList = (ScanList) scanListUnmarshaller.unmarshal(source);
			
		}  catch (Exception e) {
			e.printStackTrace();
		}
		
		return scanList;
	}

	synchronized public static Precursor parsePrecursor(String precursorAsStr) {
		Precursor prec = null;
		if(precursorAsStr == null || precursorAsStr.isBlank())
			return null;
		try {
			if( precursorUnmarshaller == null ) {
				precursorUnmarshaller = JAXBContext.newInstance(Precursor.class).createUnmarshaller();
			}
			
			SAXSource source = XercesSAXParser.getSAXSource( precursorAsStr );
			prec = (Precursor)precursorUnmarshaller.unmarshal(source);
			
		} catch (Exception e) {
			e.printStackTrace();
		}

		return prec;
	}

	synchronized public static Precursor parsePrecursorList(String precursorListAsStr) {
		Precursor prec = null;
		if(precursorListAsStr == null || precursorListAsStr.isBlank())
			return null;
		try {
			if( precursorListUnmarshaller == null ) {
				precursorListUnmarshaller = JAXBContext.newInstance(PrecursorList.class).createUnmarshaller();
			}

			SAXSource source = XercesSAXParser.getSAXSource( precursorListAsStr );
			PrecursorList precList = (PrecursorList) precursorListUnmarshaller.unmarshal(source);
			prec = ((precList.getPrecursors() == null) || precList.getPrecursors().isEmpty()) ? null : precList.getPrecursors().get(0);
		}  catch (Exception e) {
			e.printStackTrace();
		}

		return prec;
	}
	
	synchronized public static ComponentList parseComponentList(String paramTreeAsStr) {

		ComponentList paramTree = null;
		if(paramTreeAsStr == null || paramTreeAsStr.isBlank())
			return null;
		
		try {
			if( componentListUnmarshaller == null ) {
				componentListUnmarshaller = JAXBContext.newInstance(ComponentList.class).createUnmarshaller();
			}
			
			SAXSource source = XercesSAXParser.getSAXSource( paramTreeAsStr );
			paramTree = (ComponentList) componentListUnmarshaller.unmarshal(source);
			
		}  catch (Exception e) {
			e.printStackTrace();
		}
		
		return paramTree;
	}

	synchronized public static ReferencableParamGroup parseReferencableParamGroup(String paramTreeAsStr) {

		ReferencableParamGroup paramTree = null;
		if(paramTreeAsStr == null || paramTreeAsStr.isBlank())
			return null;

		try {
			if( refParamGroupUnmarshaller == null ) {
				refParamGroupUnmarshaller = JAXBContext.newInstance(ReferencableParamGroup.class).createUnmarshaller();
			}

			SAXSource source = XercesSAXParser.getSAXSource( paramTreeAsStr );
			paramTree = (ReferencableParamGroup) refParamGroupUnmarshaller.unmarshal(source);

		}  catch (Exception e) {
			e.printStackTrace();
		}

		return paramTree;
	}

}
