package flo.floGraph;

import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Observable.ModuleRenamedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;

/**
 * A module consists of a list of definitions.
 */
public class Module extends BoxDefinitionContainer implements Jsonable {

	private String name;

	public Module(final String name) {
		super(null);
		this.name = name;
	}

	public Module(final JsonObject jsonObject) {
		super(jsonObject, null);

		name = jsonObject.getString("name");
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

	/**
	 * Convert this module to JSON
	 */
	@Override
	public JsonObjectBuilder toJsonObjectBuilder() {
		return super.toJsonObjectBuilder().add("name", name);
	}
}
