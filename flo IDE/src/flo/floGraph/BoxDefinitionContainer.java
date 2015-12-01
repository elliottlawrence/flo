package flo.floGraph;

import java.util.ArrayList;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Observable.BoxDefinitionAddedEvent;
import flo.Observable.BoxDefinitionRemovedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;

/**
 * A common superclass for Modules and Box Definitions, both of which can
 * contain other Box Definitions.
 */
public abstract class BoxDefinitionContainer implements Jsonable {

	private final List<BoxDefinition> boxDefinitions;
	private final BoxDefinitionContainer parent;

	public BoxDefinitionContainer(final BoxDefinitionContainer parent) {
		this.parent = parent;
		boxDefinitions = new ArrayList<BoxDefinition>();
	}

	public BoxDefinitionContainer(final JsonObject jsonObject, final BoxDefinitionContainer parent) {
		this.parent = parent;

		boxDefinitions = new ArrayList<BoxDefinition>();
		final List<JsonObject> jsonBoxDefinitions = jsonObject.getJsonArray("boxDefinitions")
				.getValuesAs(JsonObject.class);
		jsonBoxDefinitions.forEach(jo -> boxDefinitions.add(new BoxDefinition(jo, this)));
	}

	public List<BoxDefinition> getBoxDefinitions() {
		return boxDefinitions;
	}

	/**
	 * Search for a box definition by name
	 *
	 * @param name
	 * @return The box definition or null if it doesn't exist
	 */
	public BoxDefinition getBoxDefinition(final String name) {
		for (final BoxDefinition bd : boxDefinitions)
			if (bd.getBoxInterface().getName().equals(name))
				return bd;
		return null;
	}

	/**
	 * Adds a new box definition to the module
	 *
	 * @param name
	 * @return The new box definition
	 */
	public BoxDefinition addBoxDefinition(final String name) {
		final BoxDefinition bd = new BoxDefinition(name, this);
		boxDefinitions.add(bd);

		boxDefinitionAddedObservable.notifyObservers(new BoxDefinitionAddedEvent(bd));
		return bd;
	}

	public void removeBoxDefinition(final String name) {
		removeBoxDefinition(getBoxDefinition(name));
	}

	/**
	 * Remove a box definition from a box definition container
	 *
	 * @param bd
	 */
	public void removeBoxDefinition(final BoxDefinition bd) {
		final int index = boxDefinitions.indexOf(bd);
		bd.deleteObservers();
		boxDefinitions.remove(bd);

		boxDefinitionRemovedObservable.notifyObservers(new BoxDefinitionRemovedEvent(index));
	}

	public BoxDefinitionContainer getParent() {
		return parent;
	}

	/**
	 * Returns true if this contains a certain box definition, at any level
	 *
	 * @param boxDefinition
	 * @return
	 */
	public boolean contains(final BoxDefinition boxDefinition) {
		for (final BoxDefinition bd : boxDefinitions)
			if (bd == boxDefinition || bd.contains(boxDefinition))
				return true;
		return false;
	}

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<BoxDefinitionAddedEvent> boxDefinitionAddedObservable = new Observable<BoxDefinitionAddedEvent>();
	private final Observable<BoxDefinitionRemovedEvent> boxDefinitionRemovedObservable = new Observable<BoxDefinitionRemovedEvent>();

	public void addBoxDefinitionAddedObserver(final Observer<BoxDefinitionAddedEvent> o) {
		boxDefinitionAddedObservable.addObserver(o);
	}

	public void addBoxDefinitionRemovedObserver(final Observer<BoxDefinitionRemovedEvent> o) {
		boxDefinitionRemovedObservable.addObserver(o);
	}

	public void deleteObservers() {
		boxDefinitionAddedObservable.deleteObservers();
		boxDefinitionRemovedObservable.deleteObservers();
	}

	/**
	 * Convert this box definition container to JSON
	 */
	@Override
	public JsonObjectBuilder toJsonObjectBuilder() {
		final JsonArrayBuilder boxDefinitionsBuilder = Json.createArrayBuilder();
		boxDefinitions.forEach(bd -> boxDefinitionsBuilder.add(bd.toJsonObjectBuilder()));

		return Json.createObjectBuilder().add("boxDefinitions", boxDefinitionsBuilder);
	}
}
