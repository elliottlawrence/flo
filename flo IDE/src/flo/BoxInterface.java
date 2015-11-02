package flo;

import java.util.ArrayList;

/**
 * A box is a generic representation of functions, constructors,
 * and literals. When used in expressions, these appear as "black
 * boxes", and only their interface is visible. This consists of a
 * name, a set of inputs, and an output.
 */
public class BoxInterface {

	BoxFlavor flavor;
	String name;
	ArrayList<Input> inputs;
	Output output;
	
	public BoxInterface() {
		inputs = new ArrayList<Input>();
	}
	
	public BoxFlavor getFlavor() {
		return flavor;
	}
	
	public String getName() {
		return name;
	}
	
	public ArrayList<Input> getInputs() {
		return inputs;
	}
	
	public Output getOutput() {
		return output;
	}
}