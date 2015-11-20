package flo;

import java.util.ArrayList;

import flo.Observable.BoxDefinitionSelectedEvent;
import flo.Observable.ModuleAddedEvent;
import flo.Observable.ModuleRemovedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A FloGraph is a graphical representation of a program, consisting of a list
 * of modules.
 */
public class FloGraph {

	private final String name;
	private final ArrayList<Module> modules;

	/**
	 * The box definition that is either selected in the tree or displayed in
	 * the canvas
	 */
	private BoxDefinition currentBoxDefinition;

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<ModuleAddedEvent> moduleAddedObservable = new Observable<ModuleAddedEvent>();
	private final Observable<ModuleRemovedEvent> moduleRemovedObservable = new Observable<ModuleRemovedEvent>();
	private final Observable<BoxDefinitionSelectedEvent> boxDefinitionSelectedObservable = new Observable<BoxDefinitionSelectedEvent>();

	public FloGraph(final String name) {
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
	 *
	 * @param name
	 * @return The module or null if it doesn't exist
	 */
	public Module getModule(final String name) {
		for (final Module m : modules)
			if (m.getName().equals(name))
				return m;
		return null;
	}

	/**
	 * Adds a new module to the Flo Graph
	 *
	 * @param name
	 * @return The new module
	 */
	public Module addModule(final String name) {
		final Module m = new Module(name);
		modules.add(m);

		moduleAddedObservable.notifyObservers(new ModuleAddedEvent(m));
		return m;
	}

	public void removeModule(final String name) {
		removeModule(getModule(name));
	}

	/**
	 * Removes a module from the Flo Graph
	 *
	 * @param m
	 */
	public void removeModule(final Module m) {
		final int index = modules.indexOf(m);
		modules.remove(m);

		moduleRemovedObservable.notifyObservers(new ModuleRemovedEvent(index));
	}

	public BoxDefinition getCurrentBoxDefinition() {
		return currentBoxDefinition;
	}

	public void setCurrentBoxDefinition(final BoxDefinition bd) {
		currentBoxDefinition = bd;
		boxDefinitionSelectedObservable.notifyObservers(new BoxDefinitionSelectedEvent(bd));
	}

	public void addModuleAddedObserver(final Observer<ModuleAddedEvent> o) {
		moduleAddedObservable.addObserver(o);
	}

	public void addModuleRemovedObserver(final Observer<ModuleRemovedEvent> o) {
		moduleRemovedObservable.addObserver(o);
	}

	public void addBoxDefinitionSelectedObserver(final Observer<BoxDefinitionSelectedEvent> o) {
		boxDefinitionSelectedObservable.addObserver(o);
	}
}
