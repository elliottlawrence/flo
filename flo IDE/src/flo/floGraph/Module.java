package flo.floGraph;

import flo.Observable.ModuleRenamedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A module consists of a list of definitions.
 */
public class Module extends BoxDefinitionContainer {

	private String name;

	public Module(final String name) {
		// Modules are not contained in anything (other than the FloGraph)
		super(null);
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;

		moduleRenamedObservable.notifyObservers(new ModuleRenamedEvent());
	}

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<ModuleRenamedEvent> moduleRenamedObservable = new Observable<ModuleRenamedEvent>();

	public void addModuleRenamedObserver(final Observer<ModuleRenamedEvent> o) {
		moduleRenamedObservable.addObserver(o);
	}

	@Override
	public void deleteObservers() {
		super.deleteObservers();
		moduleRenamedObservable.deleteObservers();
	}
}
