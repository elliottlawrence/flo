package flo;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

/**
 * A module consists of a list of definitions.
 */
public class Module extends Observable implements Observer {

	private String name;
	private ArrayList<BoxDefinition> boxDefinitions;
	
	/**
	 * Is this module expanded in the tree?
	 * (Sorta breaks MVC, but it's more awkward otherwise.)
	 */
	private boolean expanded = false;
	
	public Module(String name) {
		this.name = name;
		boxDefinitions = new ArrayList<BoxDefinition>();
	}
	
	public String getName() {
		return name;
	}
	
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
		BoxDefinition bd = new BoxDefinition(name, this);
		bd.addObserver(this);
		boxDefinitions.add(bd);
		
		// Make sure the corresponding tree item is expanded
		expanded = true;
		
		setChanged();
		notifyObservers();
		
		return bd;
	}
	
	public boolean getExpanded() {
		return expanded;
	}
	
	public void setExpanded(boolean expanded) {
		this.expanded = expanded;
		// No need to setChanged() since the view already reflects this
	}

	/**
	 * If a box definition changes, then the module has also changed.
	 */
	@Override
	public void update(Observable o, Object arg) {
		setChanged();
		notifyObservers(arg);
	}
}
