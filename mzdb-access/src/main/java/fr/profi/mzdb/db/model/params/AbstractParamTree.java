package fr.profi.mzdb.db.model.params;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.model.params.param.CVEntry;
import fr.profi.mzdb.db.model.params.param.UserParam;
import fr.profi.mzdb.db.model.params.param.UserText;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

/**
 * @author David Bouyssie
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractParamTree implements SerializationInterface { // implements IParamContainer

	/** The cv params. */
	protected List<CVParam> cvParams;

	/** The user params. */
	protected List<UserParam> userParams;

	/**
	 * The userText params: newly introduced for handling Thermo metadata in text field
	 */
	protected List<UserText> userTexts;


	public AbstractParamTree() {
	}



	
	public void setCvParams(List<CVParam> cvParams) {
		this.cvParams = cvParams;
	}

	public void setUserParams(List<UserParam> userParams) {
		this.userParams = userParams;
	}

	public void setUserTexts(List<UserText> userTexts) {
		this.userTexts = userTexts;
	}



	@Override
	public void write(SerializationWriter writer) throws IOException {


		boolean hasData = cvParams!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeInt32(cvParams.size());
			for (SerializationInterface serializableObject : cvParams) {
				serializableObject.write(writer);
			}
		}

		hasData = userParams!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeInt32(userParams.size());
			for (SerializationInterface serializableObject : userParams) {
				serializableObject.write(writer);
			}
		}

		hasData = userTexts!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeInt32(userTexts.size());
			for (SerializationInterface serializableObject : userTexts) {
				serializableObject.write(writer);
			}
		}

	}

	@Override
	public void read(SerializationReader reader) throws IOException {

		boolean hasData = reader.readBoolean();
		if (hasData) {
			int size = reader.readInt32();
			cvParams = new ArrayList<>(size);
			for (int i = 0; i < size; i++) {
				CVParam element = new CVParam(reader);
				cvParams.add(element);
			}
		} else {
			cvParams = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			int size = reader.readInt32();
			userParams = new ArrayList<>(size);
			for (int i = 0; i < size; i++) {
				UserParam element = new UserParam(reader);
				userParams.add(element);
			}
		} else {
			userParams = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			int size = reader.readInt32();
			userTexts = new ArrayList<>(size);
			for (int i = 0; i < size; i++) {
				UserText element = new UserText(reader);
				userTexts.add(element);
			}
		} else {
			userTexts = null;
		}
	}



}
