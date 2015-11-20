package flo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Point;

import flo.Observable.BoxAddedEvent;
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

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<BoxAddedEvent> boxAddedObservable = new Observable<BoxAddedEvent>();

	public BoxDefinition(final String name, final BoxDefinitionContainer parent) {
		boxInterface = new BoxInterface(name);
		boxes = new HashMap<Integer, Pair<BoxInterface, Point>>();
		cables = new ArrayList<Cable>();
		boxDefinitions = new ArrayList<BoxDefinition>();

		// The box definition or module this box definition is contained in
		this.parent = parent;
	}

	public BoxInterface getBoxInterface() {
		return boxInterface;
	}

	public Map<Integer, Pair<BoxInterface, Point>> getBoxes() {
		return boxes;
	}

	public void addBox(final BoxInterface bi) {
		boxes.put(boxes.size(), new Pair<BoxInterface, Point>(bi, new Point(10, 10)));

		boxAddedObservable.notifyObservers(new BoxAddedEvent());
	}

	public ArrayList<Cable> getCables() {
		return cables;
	}

	public void addBoxAddedObserver(final Observer<BoxAddedEvent> o) {
		boxAddedObservable.addObserver(o);
	}
}
