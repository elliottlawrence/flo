package flo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

/**
 * A box definition consists of the box's interface augmented with a
 * set of boxes and cables connecting nodes on the boxes. In addition,
 * a box may have local box definitions, akin to a let/where clause
 * in Haskell.
 */
public class BoxDefinition extends Observable implements Observer {
	
	private BoxInterface boxInterface;
	private Map<Integer, BoxInterface> boxes;
	private ArrayList<Cable> cables;
	private ArrayList<BoxDefinition> localDefinitions;
	
	/**
	 * The Module or Box Definition where this Box is defined
	 */
	private Object parent;
	
	/**
	 * Is this module expanded in the tree?
	 * (Sorta breaks MVC, but it's more awkward otherwise.)
	 */
	private boolean expanded = false;
	
	public BoxDefinition(String name, Object parent) {
		boxInterface = new BoxInterface(name);
		boxes = new HashMap<Integer, BoxInterface>();
		cables = new ArrayList<Cable>();
		localDefinitions = new ArrayList<BoxDefinition>();
		
		this.parent = parent;
	}
	
	public BoxInterface getBoxInterface() {
		return boxInterface;
	}
	
	public Map<Integer, BoxInterface> getBoxes() {
		return boxes;
	}
	
	public ArrayList<Cable> getCables() {
		return cables;
	}
	
	public ArrayList<BoxDefinition> getLocalDefinitions() {
		return localDefinitions;
	}
	
	/**
	 * Search for a local box definition by name
	 * @param name
	 * @return The local box definition or null if it doesn't exist
	 */
	public BoxDefinition getLocalDefinition(String name) {
		for (BoxDefinition bd : localDefinitions) {
			if (bd.getBoxInterface().getName().equals(name)) {
				return bd;
			}
		}
		return null;
	}
	
	/**
	 * Add a new local box definition to this box definition
	 * @param name
	 * @return The new box definition
	 */
	public BoxDefinition addLocalDefinition(String name) {
		BoxDefinition bd = new BoxDefinition(name, this);
		bd.addObserver(this);
		localDefinitions.add(bd);
		
		// Make sure the corresponding box definition is expanded
		expanded = true;
		
		setChanged();
		notifyObservers();
		
		return bd;
	}

	public Object getParent() {
		return parent;
	}
	
	public boolean getExpanded() {
		return expanded;
	}
	
	public void setExpanded(boolean expanded) {
		this.expanded = expanded;
		// No need to setChanged() since the view already reflects this
	}

	/**
	 * If a local box definition changes, then the surrounding
	 * box definition has also changed.
	 */
	@Override
	public void update(Observable o, Object arg) {
		setChanged();
		notifyObservers(arg);
	}
}
