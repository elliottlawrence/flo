package flo;

import java.util.ArrayList;

/**
 * A module consists of a list of definitions.
 */
public class Module {

	String name;
	ArrayList<BoxDefinition> boxDefinitions;
	
	public Module() {
		boxDefinitions = new ArrayList<BoxDefinition>();
	}
	
	public String getName() {
		return name;
	}
	
	public ArrayList<BoxDefinition> getBoxDefinitions() {
		return boxDefinitions;
	}
}
