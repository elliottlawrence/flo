package flo.floGraph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Point;

import flo.Pair;
import flo.Observable.BoxAddedEvent;
import flo.Observable.CurrentBoxDefinitionEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A box definition consists of the box's interface augmented with a set of
 * boxes and cables connecting nodes on the boxes. In addition, a box may have
 * local box definitions, akin to a let/where clause in Haskell.
 */
public class BoxDefinition extends BoxDefinitionContainer {

	private final BoxInterface boxInterface;
	private final Map<Integer, Pair<BoxInterface, Point>> boxes;
	private final ArrayList<Cable> cables;

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

	public void addBox(final BoxInterface bi) {
		boxes.put(getUniqueID(), new Pair<BoxInterface, Point>(bi, new Point(100, 100)));

		boxAddedObservable.notifyObservers(new BoxAddedEvent());
		currentBoxDefinitionObservable.notifyObservers(new CurrentBoxDefinitionEvent());
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

	public ArrayList<Cable> getCables() {
		return cables;
	}

	public void addCable(final Cable cable) {
		cables.add(cable);
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
}
