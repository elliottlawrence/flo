package flo.floGraph;

/**
 * A cable connects the output of one box to the input of another.
 */
public class Cable {

	private final Output output;
	private final Input input;

	public Cable(final Output output, final Input input) {
		this.output = output;
		this.input = input;
	}

	public Input getInput() {
		return input;
	}

	public Output getOutput() {
		return output;
	}

}
