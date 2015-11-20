package flo;

import java.util.ArrayList;

import flo.Observable.BoxInterfaceRenamedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;

/**
 * A box is a generic representation of functions, constructors, and literals.
 * When used in expressions, these appear as "black boxes", and only their
 * interface is visible. This consists of a name, a set of inputs, and an
 * output.
 */
public class BoxInterface {

	private BoxFlavor flavor;
	private String name;
	private final ArrayList<Input> inputs;
	private Output output;

	/**
	 * Observables corresponding to the different events this object can emit
	 */
	private final Observable<BoxInterfaceRenamedEvent> boxInterfaceRenamedObservable = new Observable<BoxInterfaceRenamedEvent>();

	public BoxInterface(final String name) {
		this.name = name;
		inputs = new ArrayList<Input>();
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
	}

	public ArrayList<Input> getInputs() {
		return inputs;
	}

	public void addInput(final String name) {
		inputs.add(new Input(name));
	}

	public Output getOutput() {
		return output;
	}

	public void addBoxInterfaceRenamedObserver(final Observer<BoxInterfaceRenamedEvent> o) {
		boxInterfaceRenamedObservable.addObserver(o);
	}
}