package fr.profi.mzdb.db.model.params;

import java.util.List;

import javax.xml.bind.annotation.*;

/**
 * @author Marco
 * 
 */
@XmlRootElement(name = "componentList")
public class ComponentList extends AbstractParamTree {

	protected List<Component> components;

	protected int count;

	public ComponentList(int c) {
		this.count = c;
	}

	public ComponentList() {
	}

	@XmlElements({
					@XmlElement(name = "detector", required = true, type = DetectorComponent.class),
					@XmlElement(name = "analyzer", required = true, type = AnalyzerComponent.class),
					@XmlElement(name = "source", required = true, type = SourceComponent.class)
	})

	public List<Component> getComponents() {
		return components;
	}

	@XmlAttribute(required = true)
	@XmlSchemaType(name = "nonNegativeInteger")
	public int getCount(){
		return  count;
	}

	public void setCount(int c){
		count = c;
	}

	public void setComponents(List<Component> components){
		this.components = components;
	}
}
