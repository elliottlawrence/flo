package flo.floGraph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;

import org.eclipse.swt.graphics.Point;

import flo.Observable.BoxAddedEvent;
import flo.Observable.CurrentBoxDefinitionEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;
import flo.Util.Pair;

/**
 * A box definition consists of the box's interface augmented with a set of
 * boxes and cables connecting nodes on the boxes. In addition, a box may have
 * local box definitions, akin to a let/where clause in Haskell.
 */
public class BoxDefinition extends BoxDefinitionContainer implements Jsonable {

	private final BoxInterface boxInterface;
	private final Map<Integer, Pair<BoxInterface, Point>> boxes;
	private final List<Cable> cables;

	public BoxDefinition(final String name, final BoxDefinitionContainer parent) {
		super(parent);
		boxInterface = new BoxInterface(name);
		boxes = new HashMap<Integer, Pair<BoxInterface, Point>>();
		cables = new ArrayList<Cable>();
	}

	public BoxInterface getBoxInterface() {
		return boxInterface;
	}

	public Map<Integer, Pair<BoxInterface, Point>> getBoxes() {
		return boxes;
	}

	public int addBox(final BoxInterface bi) {
		final int ID = getUniqueID();
		bi.setID(ID);
		boxes.put(ID, new Pair<BoxInterface, Point>(bi, new Point(100, 100)));

		boxAddedObservable.notifyObservers(new BoxAddedEvent());
		currentBoxDefinitionObservable.notifyObservers(new CurrentBoxDefinitionEvent());

		return ID;
	}

	public void removeBox(final int ID) {
		final BoxInterface bi = boxes.get(ID).x;

		// Remove any cables attached to box
		for (final Input i : bi.getInputs())
			if (i.hasCable())
				removeCable(i.getCable());

		final Output o = bi.getOutput();
		if (o.hasCable()) {
			final List<Cable> cablesToRemove = new ArrayList<Cable>(o.getCables());
			for (final Cable c : cablesToRemove)
				removeCable(c);
		}

		boxes.remove(ID);
	}

	private int getUniqueID() {
		for (int i = 0;; i++)
			if (!boxes.keySet().contains(i))
				return i;
	}

	public void setBoxLocation(final Integer ID, final Point point) {
		final BoxInterface bi = boxes.get(ID).x;
		boxes.put(ID, new Pair<BoxInterface, Point>(bi, point));

		currentBoxDefinitionObservable.notifyObservers(new CurrentBoxDefinitionEvent());
	}

	public List<Cable> getCables() {
		return cables;
	}

	public void addCable(final Cable cable) {
		cables.add(cable);
	}

	public void removeCable(final Cable cable) {
		cable.deleteConnections();
		cables.remove(cable);
	}

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<BoxAddedEvent> boxAddedObservable = new Observable<BoxAddedEvent>();
	private final Observable<CurrentBoxDefinitionEvent> currentBoxDefinitionObservable = new Observable<CurrentBoxDefinitionEvent>();

	public void addBoxAddedObserver(final Observer<BoxAddedEvent> o) {
		boxAddedObservable.addObserver(o);
	}

	public void addCurrentBoxDefinitionObserver(final Observer<CurrentBoxDefinitionEvent> o) {
		currentBoxDefinitionObservable.addObserver(o);
	}

	@Override
	public void deleteObservers() {
		super.deleteObservers();
		boxAddedObservable.deleteObservers();
		currentBoxDefinitionObservable.deleteObservers();
	}

	public void deleteCurrentBoxDefinitionObserver(final Observer<CurrentBoxDefinitionEvent> o) {
		currentBoxDefinitionObservable.deleteObserver(o);
	}

	/**
	 * Convert this module to JSON
	 */
	@Override
	public JsonObjectBuilder toJsonObjectBuilder() {
		final JsonArrayBuilder boxBuilder = Json.createArrayBuilder();
		for (final Integer i : boxes.keySet()) {
			final Pair<BoxInterface, Point> pair = boxes.get(i);
			final JsonObjectBuilder objectBuilder = Json.createObjectBuilder().add("ID", i)
					.add("boxInterface", pair.x.toJsonObjectBuilder()).add("x", pair.y.x).add("y", pair.y.y);
			boxBuilder.add(objectBuilder);
		}

		final JsonArrayBuilder cableBuilder = Json.createArrayBuilder();
		for (final Cable c : cables)
			cableBuilder.add(c.toJsonObjectBuilder());

		return super.toJsonObjectBuilder().add("boxInterface", boxInterface.toJsonObjectBuilder())
				.add("boxes", boxBuilder).add("cables", cableBuilder);
	}
}
