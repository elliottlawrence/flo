package flo;

import java.util.ArrayList;
import java.util.Observable;

/**
 * A box is a generic representation of functions, constructors, and literals.
 * When used in expressions, these appear as "black boxes", and only their
 * interface is visible. This consists of a name, a set of inputs, and an
 * output.
 */
public class BoxInterface extends Observable {

	private BoxFlavor flavor;
	private String name;
	private final ArrayList<Input> inputs;
	private Output output;

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

		setChanged();
		notifyObservers(new Object[] { FloGraphChange.BoxInterfaceRenamed });
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
}