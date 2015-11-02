package flo;

import java.util.ArrayList;

/**
 * A FloGraph is a graphical representation of a program, consisting
 * of a list of modules.
 */
public class FloGraph {
	
	private String name;
	private ArrayList<Module> modules;
	
	public FloGraph() {
		modules = new ArrayList<Module>();
	}

	public String getName() {
		return name;
	}
	
	public ArrayList<Module> getModules() {
		return modules;
	}
}
