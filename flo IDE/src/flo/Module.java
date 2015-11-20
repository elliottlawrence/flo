package flo;

import java.util.ArrayList;

import flo.Observable.ModuleRenamedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A module consists of a list of definitions.
 */
public class Module extends BoxDefinitionContainer {

	private String name;

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<ModuleRenamedEvent> moduleRenamedObservable = new Observable<ModuleRenamedEvent>();

	public Module(final String name) {
		this.name = name;
		boxDefinitions = new ArrayList<BoxDefinition>();

		// Modules are not contained in anything (other than the FloGraph)
		parent = null;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;

		moduleRenamedObservable.notifyObservers(new ModuleRenamedEvent());
	}

	public void addModuleRenamedObserver(final Observer<ModuleRenamedEvent> o) {
		moduleRenamedObservable.addObserver(o);
	}
}
