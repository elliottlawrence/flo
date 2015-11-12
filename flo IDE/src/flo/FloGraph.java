package flo;

import java.util.ArrayList;
import java.util.Observable;

/**
 * A FloGraph is a graphical representation of a program, consisting
 * of a list of modules.
 */
public class FloGraph extends Observable {
	
	private String name;
	private ArrayList<Module> modules;
	
	/**
	 * The box definition that is either selected in the tree or
	 * displayed in the canvas
	 */
	private BoxDefinition currentBoxDefinition;
	
	public FloGraph(String name) {
		this.name = name;
		modules = new ArrayList<Module>();
	}

	public String getName() {
		return name;
	}
	
	public ArrayList<Module> getModules() {
		return modules;
	}
	
	/**
	 * Search for a module by name
	 * @param name
	 * @return The module or null if it doesn't exist
	 */
	public Module getModule(String name) {
		for (Module m : modules) {
			if (m.getName().equals(name)) {
				return m;
			}
		}
		return null;
	}
	
	/**
	 * Adds a new module to the Flo Graph
	 * @param name
	 * @return The new module
	 */
	public Module addModule(String name) {
		Module m = new Module(name);
		modules.add(m);
		
		setChanged();
		notifyObservers(new Object[] {FloGraphChange.ModuleAdded, m});
		
		return m;
	}
	
	public void removeModule(String name) {
		removeModule(getModule(name));
	}
	
	/**
	 * Removes a module from the Flo Graph
	 * @param m
	 */
	public void removeModule(Module m) {
		int index = modules.indexOf(m);
		modules.remove(m);
		
		setChanged();
		notifyObservers(new Object[] {FloGraphChange.ModuleRemoved, index});
	}
	
	public BoxDefinition getCurrentBoxDefinition() {
		return currentBoxDefinition;
	}
	
	public void setCurrentBoxDefinition(BoxDefinition bd) {
		currentBoxDefinition = bd;
		
		setChanged();
		notifyObservers(new Object[] {FloGraphChange.BoxDefinitionSelected, bd});
	}
	
}
