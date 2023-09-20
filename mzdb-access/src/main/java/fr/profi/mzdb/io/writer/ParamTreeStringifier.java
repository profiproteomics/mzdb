package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.db.model.params.ComponentList;
import fr.profi.mzdb.db.model.params.FileContentParams;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.db.model.params.ReferencableParamGroup;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import java.io.StringWriter;
import java.io.Writer;

// TODO: Auto-generated Javadoc

/**
 * The Class ParamTreeParser.
 * 
 * @author David Bouyssie
 */
public class ParamTreeStringifier {
	
	/** The xml mappers. */
	public static Marshaller paramTreeMarshaller = null;
	public static Marshaller componentMarshaller = null;
	public static Marshaller fileContentMarshaller = null;
	public static Marshaller refParamGroupMarshaller = null;

	synchronized public static String stringifyParamTree(ParamTree paramTree) {

		String paramTreeAsStr ="";
		if(paramTree == null)
			return paramTreeAsStr;


		try {
			if( paramTreeMarshaller == null ) {
				paramTreeMarshaller = JAXBContext.newInstance(ParamTree.class).createMarshaller();
				paramTreeMarshaller.setProperty(Marshaller.JAXB_FRAGMENT, Boolean.TRUE);
			}

			Writer w = new StringWriter();
			paramTreeMarshaller.marshal(paramTree, w);

			paramTreeAsStr = w.toString();
			return paramTreeAsStr;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return paramTreeAsStr;
	}

	synchronized public static String stringifyFileContentParam(FileContentParams paramTree) {

		String paramTreeAsStr ="";
		if(paramTree == null)
			return paramTreeAsStr;

		try {
			if( fileContentMarshaller == null ) {
				fileContentMarshaller = JAXBContext.newInstance(FileContentParams.class).createMarshaller();
				fileContentMarshaller.setProperty(Marshaller.JAXB_FRAGMENT, Boolean.TRUE);
			}

			Writer w = new StringWriter();
			fileContentMarshaller.marshal(paramTree, w);
			paramTreeAsStr = w.toString();
			return paramTreeAsStr;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return paramTreeAsStr;
	}

	synchronized public static String stringifyRefParamGroup(ReferencableParamGroup paramGroup) {

		String paramTreeAsStr ="";
		if(paramGroup == null)
			return paramTreeAsStr;

		try {
			if( refParamGroupMarshaller == null ) {
				refParamGroupMarshaller = JAXBContext.newInstance(ReferencableParamGroup.class).createMarshaller();
				refParamGroupMarshaller.setProperty(Marshaller.JAXB_FRAGMENT, Boolean.TRUE);
			}

			Writer w = new StringWriter();
			refParamGroupMarshaller.marshal(paramGroup, w);
			paramTreeAsStr = w.toString();
			return paramTreeAsStr;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return paramTreeAsStr;
	}

	synchronized public static String stringifyComponentList(ComponentList componentList) {

		String compoListAsStr ="";
		if(compoListAsStr == null)
			return compoListAsStr;

		try {
			if( componentMarshaller == null ) {
				componentMarshaller = JAXBContext.newInstance(ComponentList.class).createMarshaller();
				componentMarshaller.setProperty(Marshaller.JAXB_FRAGMENT, Boolean.TRUE);
			}

			Writer w = new StringWriter();
			componentMarshaller.marshal(componentList, w);
			compoListAsStr = w.toString();
			return compoListAsStr;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return compoListAsStr;
	}


}
