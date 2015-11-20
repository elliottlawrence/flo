package flo.floGraph;

import java.util.ArrayList;

import flo.Observable.BoxDefinitionAddedEvent;
import flo.Observable.BoxDefinitionRemovedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A common superclass for Modules and Box Definitions, both of which can
 * contain other Box Definitions.
 */
public abstract class BoxDefinitionContainer {

	private final ArrayList<BoxDefinition> boxDefinitions;
	private final BoxDefinitionContainer parent;

	public BoxDefinitionContainer(final BoxDefinitionContainer parent) {
		boxDefinitions = new ArrayList<BoxDefinition>();
		this.parent = parent;
	}

	public ArrayList<BoxDefinition> getBoxDefinitions() {
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
}
