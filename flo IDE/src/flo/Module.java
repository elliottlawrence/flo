package flo;

import java.util.ArrayList;

/**
 * A module consists of a list of definitions.
 */
public class Module extends BoxDefinitionContainer {

	private String name;
	
	public Module(String name) {
		this.name = name;
		boxDefinitions = new ArrayList<BoxDefinition>();
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
		
		setChanged();
		notifyObservers(new Object[] {FloGraphChange.ModuleRenamed});
	}
}
