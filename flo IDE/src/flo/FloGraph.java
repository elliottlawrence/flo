package flo;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

/**
 * A FloGraph is a graphical representation of a program, consisting
 * of a list of modules.
 */
public class FloGraph extends Observable implements Observer {
	
	private String name;
	private ArrayList<Module> modules;
	
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
		m.addObserver(this);
		modules.add(m);
		
		setChanged();
		notifyObservers();
		
		return m;
	}

	/**
	 * If a module changes, then the Flo Graph has also changed.
	 */
	@Override
	public void update(Observable o, Object arg) {
		setChanged();
		notifyObservers(arg);
	}
}
