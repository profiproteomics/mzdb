package fr.profi.mzdb.db.model.params;

import java.io.IOException;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.model.params.param.UserParam;
import fr.profi.mzdb.db.model.params.param.UserText;
import fr.profi.mzdb.serialization.SerializationReader;

/**
 * The Class ParamTree.
 * 
 * @author David Bouyssie
 */
@XmlRootElement(name = "params")
public class ParamTree extends AbstractNoXMLParamTree {

	public ParamTree() {
	}

	public ParamTree(SerializationReader reader) throws IOException {
		read(reader);
	}


	@Override
	@XmlElement(name = "cvParam", type = CVParam.class, required = false)
	@XmlElementWrapper(name = "cvParams")
	public List<CVParam> getCVParams() {
		return super.getCVParams();
	}

	@Override
	@XmlElement(name = "userParam", type = UserParam.class, required = false)
	@XmlElementWrapper(name = "userParams")
	public List<UserParam> getUserParams() {
		return super.getUserParams();
	}

	@Override
	@XmlElement(name = "userText", type = UserText.class, required = false)
	@XmlElementWrapper(name = "userTexts")
	public List<UserText> getUserTexts() {
		return super.getUserTexts();
	}

}
