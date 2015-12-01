package flo.floGraph;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonReader;
import javax.json.JsonWriter;

import flo.Observable.BoxDefinitionSelectedEvent;
import flo.Observable.CurrentBoxDefinitionEvent;
import flo.Observable.ModuleAddedEvent;
import flo.Observable.ModuleRemovedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;

/**
 * A FloGraph is a graphical representation of a program, consisting of a list
 * of modules.
 */
public class FloGraph implements Jsonable {

	private String name;
	private final List<Module> modules;

	/**
	 * The box definition that is either selected in the tree or displayed in
	 * the canvas
	 */
	private BoxDefinition currentBoxDefinition;

	public FloGraph(final String name) {
		this(name, new ArrayList<Module>());
	}

	public FloGraph(final String name, final List<Module> modules) {
		this.name = name;
		this.modules = modules;
	}

	public FloGraph(final JsonObject jsonObject) {
		name = jsonObject.getString("name");

		modules = new ArrayList<Module>();
		final List<JsonObject> jsonModules = jsonObject.getJsonArray("modules").getValuesAs(JsonObject.class);
		jsonModules.forEach(jo -> modules.add(new Module(jo)));
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public List<Module> getModules() {
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
		m.deleteObservers();
		modules.remove(m);

		moduleRemovedObservable.notifyObservers(new ModuleRemovedEvent(index));
	}

	public BoxDefinition getCurrentBoxDefinition() {
		return currentBoxDefinition;
	}

	public void setCurrentBoxDefinition(final BoxDefinition bd) {
		// Remove this from the old box definition's observers
		if (currentBoxDefinition != null) {
			currentBoxDefinition.deleteCurrentBoxDefinitionObserver(currentBoxDefinitionObserver);
			currentBoxDefinition.getBoxInterface().deleteCurrentBoxDefinitionObserver(currentBoxDefinitionObserver);
		}

		// Set the new box definition
		currentBoxDefinition = bd;

		// Add this as an observer to the new box definition
		if (currentBoxDefinition != null) {
			currentBoxDefinition.addCurrentBoxDefinitionObserver(currentBoxDefinitionObserver);
			currentBoxDefinition.getBoxInterface().addCurrentBoxDefinitionObserver(currentBoxDefinitionObserver);
		}

		boxDefinitionSelectedObservable.notifyObservers(new BoxDefinitionSelectedEvent(bd));
		currentBoxDefinitionObservable.notifyObservers(new CurrentBoxDefinitionEvent());
	}

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<ModuleAddedEvent> moduleAddedObservable = new Observable<ModuleAddedEvent>();
	private final Observable<ModuleRemovedEvent> moduleRemovedObservable = new Observable<ModuleRemovedEvent>();
	private final Observable<BoxDefinitionSelectedEvent> boxDefinitionSelectedObservable = new Observable<BoxDefinitionSelectedEvent>();

	/**
	 * This observable emits events any time the current box definition changes.
	 */
	private final Observable<CurrentBoxDefinitionEvent> currentBoxDefinitionObservable = new Observable<CurrentBoxDefinitionEvent>();
	private final Observer<CurrentBoxDefinitionEvent> currentBoxDefinitionObserver = e -> currentBoxDefinitionObservable
			.notifyObservers(new CurrentBoxDefinitionEvent());

	public void addModuleAddedObserver(final Observer<ModuleAddedEvent> o) {
		moduleAddedObservable.addObserver(o);
	}

	public void addModuleRemovedObserver(final Observer<ModuleRemovedEvent> o) {
		moduleRemovedObservable.addObserver(o);
	}

	public void addBoxDefinitionSelectedObserver(final Observer<BoxDefinitionSelectedEvent> o) {
		boxDefinitionSelectedObservable.addObserver(o);
	}

	public void addCurrentBoxDefinitionObserver(final Observer<CurrentBoxDefinitionEvent> o) {
		currentBoxDefinitionObservable.addObserver(o);
	}

	public void deleteObservers() {
		moduleAddedObservable.deleteObservers();
		moduleRemovedObservable.deleteObservers();
		boxDefinitionSelectedObservable.deleteObservers();
		currentBoxDefinitionObservable.deleteObservers();
	}

	/**
	 * Convert this Flo Graph to JSON
	 */
	@Override
	public JsonObjectBuilder toJsonObjectBuilder() {
		final JsonArrayBuilder modulesBuilder = Json.createArrayBuilder();
		modules.forEach(m -> modulesBuilder.add(m.toJsonObjectBuilder()));

		return Json.createObjectBuilder().add("name", name).add("modules", modulesBuilder);
	}

	/**
	 * Saves the file
	 */
	public void save(final String path) {
		try {
			final JsonWriter writer = Json.createWriter(new FileOutputStream(path));
			writer.writeObject(toJsonObjectBuilder().build());
			writer.close();
		} catch (final FileNotFoundException e1) {
			e1.printStackTrace();
		}
	}

	/**
	 * Open a file from the given path
	 */
	public static FloGraph open(final String path) {
		JsonObject jsonObject = null;
		try {
			final JsonReader reader = Json.createReader(new FileInputStream(path));
			jsonObject = reader.readObject();
			reader.close();
		} catch (final FileNotFoundException e) {
			e.printStackTrace();
		}

		if (jsonObject == null)
			return null;

		return new FloGraph(jsonObject);
	}

}
