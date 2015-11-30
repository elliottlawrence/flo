package flo.floGraph;

import java.util.ArrayList;

import flo.Observable.BoxInterfaceRenamedEvent;
import flo.Observable.CurrentBoxDefinitionEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A box is a generic representation of functions, constructors, and literals.
 * When used in expressions, these appear as "black boxes", and only their
 * interface is visible. This consists of a name, a set of inputs, and an
 * output. (For symmetry, there is an end input connected to the output so that
 * the last box's output can be connected to an input.)
 */
public class BoxInterface {

	private BoxFlavor flavor;
	private String name;
	private final ArrayList<Input> inputs;
	private final Output output;
	private final Input endInput;

	public BoxInterface(final String name) {
		this.name = name;
		inputs = new ArrayList<Input>();
		output = new Output();
		endInput = new Input(name, this);
	}

	public BoxFlavor getFlavor() {
		return flavor;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;

		boxInterfaceRenamedObservable.notifyObservers(new BoxInterfaceRenamedEvent());
		currentBoxDefinitionObservable.notifyObservers(new CurrentBoxDefinitionEvent());
	}

	public ArrayList<Input> getInputs() {
		return inputs;
	}

	public boolean containsInput(final String name) {
		return getInput(name) != null;
	}

	public void addInput(final String name) {
		inputs.add(new Input(name, this));
	}

	public void removeInput(final Input input) {
		inputs.remove(input);

		// Remove cables attached to input
		final Cable cable = input.getCable();
		if (cable != null)
			cable.getParent().removeCable(cable);
	}

	public Input getInput(final String name) {
		for (final Input i : inputs)
			if (i.getName().equals(name))
				return i;
		return null;
	}

	public void removeInput(final String name) {
		inputs.remove(getInput(name));
	}

	public Output getOutput() {
		return output;
	}

	public Input getEndInput() {
		return endInput;
	}

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<BoxInterfaceRenamedEvent> boxInterfaceRenamedObservable = new Observable<BoxInterfaceRenamedEvent>();
	private final Observable<CurrentBoxDefinitionEvent> currentBoxDefinitionObservable = new Observable<CurrentBoxDefinitionEvent>();

	public void addBoxInterfaceRenamedObserver(final Observer<BoxInterfaceRenamedEvent> o) {
		boxInterfaceRenamedObservable.addObserver(o);
	}

	public void addCurrentBoxDefinitionObserver(final Observer<CurrentBoxDefinitionEvent> o) {
		currentBoxDefinitionObservable.addObserver(o);
	}

	public void deleteObservers() {
		boxInterfaceRenamedObservable.deleteObservers();
		currentBoxDefinitionObservable.deleteObservers();
	}

	public void deleteCurrentBoxDefinitionObserver(final Observer<CurrentBoxDefinitionEvent> o) {
		currentBoxDefinitionObservable.deleteObserver(o);
	}

}