package flo;

import java.util.ArrayList;
import java.util.Observable;

/**
 * A common superclass for Modules and Box Definitions, both of which can
 * contain other Box Definitions.
 */
public abstract class BoxDefinitionContainer extends Observable {

	protected ArrayList<BoxDefinition> boxDefinitions;
	
	public ArrayList<BoxDefinition> getBoxDefinitions() {
		return boxDefinitions;
	}
	
	/**
	 * Search for a box definition by name
	 * @param name
	 * @return The box definition or null if it doesn't exist
	 */
	public BoxDefinition getBoxDefinition(String name) {
		for (BoxDefinition bd : boxDefinitions) {
			if (bd.getBoxInterface().getName().equals(name)) {
				return bd;
			}
		}
		return null;
	}

	/**
	 * Adds a new box definition to the module
	 * @param name
	 * @return The new box definition
	 */
	public BoxDefinition addBoxDefinition(String name) {
		BoxDefinition bd = new BoxDefinition(name);
		boxDefinitions.add(bd);
		
		setChanged();
		notifyObservers(new Object[] {FloGraphChange.BoxDefinitionAdded, bd});
		
		return bd;
	}
	
	public void removeBoxDefinition(String name) {
		removeBoxDefinition(getBoxDefinition(name));
	}
	
	/**
	 * Remove a box definition from a box definition container
	 * @param bd
	 */
	public void removeBoxDefinition(BoxDefinition bd) {
		int index = boxDefinitions.indexOf(bd);
		boxDefinitions.remove(bd);
		
		setChanged();
		notifyObservers(new Object[] {FloGraphChange.BoxDefinitionRemoved, index});
	}
}
